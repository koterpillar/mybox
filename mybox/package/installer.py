import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Generic, Optional, Protocol, TypeVar

import trio

from ..driver import Driver, RunResultOutput
from ..utils import async_cached, async_cached_lock


class Installer(metaclass=ABCMeta):
    def __init__(self, driver: Driver) -> None:
        self.driver = driver
        super().__init__()

    @abstractmethod
    async def install(self, package: str) -> None:
        pass

    @abstractmethod
    async def upgrade(self, package: str) -> None:
        pass

    @abstractmethod
    async def installed_version(self, package: str) -> Optional[str]:
        pass

    @abstractmethod
    async def latest_version(self, package: str) -> str:
        pass


class PVP(Protocol):
    @property
    def installed(self) -> Optional[str]: ...
    @property
    def latest(self) -> str: ...


PV = TypeVar("PV", bound=PVP)


@dataclass
class PackageVersionInfo:
    installed: Optional[str]
    latest: str


class PackageCacheInstaller(Generic[PV], Installer, metaclass=ABCMeta):
    cache: Optional[dict[str, PV]]

    def __init__(self, driver: Driver) -> None:
        self.cache = None
        self.lock = trio.Lock()
        super().__init__(driver)

    @abstractmethod
    async def get_package_info(self, package: Optional[str]) -> dict[str, PV]:
        pass

    async def package_info(self, package: str) -> PV:
        async with self.lock:
            if self.cache is None:
                self.cache = await self.get_package_info(None)

        async with self.lock:
            if package not in self.cache:
                self.cache.update(await self.get_package_info(package))

        try:
            return self.cache[package]
        except KeyError:
            raise ValueError(f"Unknown package: {package}") from None

    async def installed_version(self, package: str) -> Optional[str]:
        info = await self.package_info(package)
        return info.installed

    async def latest_version(self, package: str) -> str:
        info = await self.package_info(package)
        return info.latest

    async def invalidate_cache(self, package: str) -> None:
        async with self.lock:
            if self.cache:
                self.cache.pop(package, None)

    async def install(self, package: str) -> None:
        await super().install(package)
        await self.invalidate_cache(package)

    async def upgrade(self, package: str) -> None:  # pragma: no cover
        # Can't install an old version of a package in tests
        await super().upgrade(package)
        await self.invalidate_cache(package)


@dataclass
class BrewPackageVersionInfo:
    """
    Brew installer prefers casks to formulae with the same name. Brew itself
    reports both separately, but it requires specifying homebrew/cask/
    everywhere so it's easier for the users to assume a cask if one exists.
    """

    cask: Optional[PackageVersionInfo]
    cask_known: bool  # Whether we specifically checked for a cask
    formula: Optional[PackageVersionInfo]

    @staticmethod
    def formula_version(version: str) -> str:
        """
        To distinguish a cask from a formula of the same name, especially if
        they have the same versions, prepend the formula version with
        "formula_".
        """
        return f"formula_{version}"

    @property
    def installed(self) -> Optional[str]:
        """
        Report installed state of both cask and formula so that any state but
        "cask is installed and formula isn't" results in a different version.
        """
        cask_installed = self.cask.installed if self.cask else None
        formula_installed = (
            self.formula_version(self.formula.installed)
            if self.formula and self.formula.installed
            else None
        )

        if cask_installed and formula_installed:
            return f"{cask_installed} {formula_installed}"
        return cask_installed or formula_installed

    @property
    def latest(self) -> str:
        if not self.cask_known:
            raise ValueError("Cask version is unknown.")  # pragma: no cover
        if self.cask:
            return self.cask.latest
        if self.formula:
            return self.formula_version(self.formula.latest)
        raise ValueError("No cask or formula info.")  # pragma: no cover


class Brew(PackageCacheInstaller[BrewPackageVersionInfo]):
    async def tap(self, repo: str) -> None:
        await self.driver.run(await self.brew(), "tap", repo)

    async def tapped(self) -> set[str]:
        output = await self.driver.run_output(await self.brew(), "tap")
        return set(output.strip().splitlines())

    async def install(self, package: str) -> None:
        info = await self.package_info(package)
        if info.cask:
            await self.driver.run(await self.brew(), "install", "--cask", package)
        else:
            await self.driver.run(await self.brew(), "install", package)
        await super().install(package)

    async def upgrade(self, package: str) -> None:
        info = await self.package_info(package)
        if not info.installed:
            raise ValueError(
                "Expected package to be installed when upgrading."
            )  # pragma: no cover
        if info.cask:
            if info.formula and info.formula.installed:
                # We want a cask, not a formula, uninstall the formula
                await self.driver.run(await self.brew(), "uninstall", package)
            if info.cask.installed:
                # Both formula and cask were installed; formula was uninstalled,
                # upgrade the cask if needed
                if info.cask.latest != info.cask.installed:
                    # Can't install an old version of a package in tests
                    await self.driver.run(
                        await self.brew(), "upgrade", "--cask", package
                    )  # pragma: no cover
            else:
                # Formula was installed (thus upgrading), but cask wasn't
                await self.driver.run(await self.brew(), "install", "--cask", package)
        else:
            # Can't install an old version of a package in tests
            await self.driver.run(
                await self.brew(), "upgrade", package
            )  # pragma: no cover
        await super().upgrade(package)

    @async_cached
    async def brew(self) -> str:
        # https://docs.brew.sh/FAQ#why-is-the-default-installation-prefix-opthomebrew-on-apple-silicon
        return await self.driver.find_executable("brew", "/opt/homebrew/bin/brew")

    @async_cached_lock
    async def brew_update(self) -> None:
        await self.driver.run(await self.brew(), "update")

    async def brew_info(self, *args: str) -> RunResultOutput:
        return await self.driver.run_output_(
            await self.brew(), "info", "--json=v2", *args
        )

    async def brew_info_cask(self, cask: str) -> RunResultOutput:
        return await self.brew_info(f"homebrew/cask/{cask}")

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, BrewPackageVersionInfo]:
        await self.brew_update()

        brew_result: RunResultOutput
        if not package:
            brew_result = await self.brew_info("--installed")
        elif "/" not in package:
            # When a bare package name is given (without homebrew/, etc. prefix),
            # try to look up the cask first.
            brew_result = await self.brew_info_cask(package)
            if not brew_result.ok:
                brew_result = await self.brew_info(package)
        else:
            brew_result = await self.brew_info(package)

        return await self.fill_package_info(brew_result)

    async def package_info(self, package: str) -> BrewPackageVersionInfo:
        """
        When only a formula version of a package is known (e.g. from
        --installed), try to look up the cask version too before returning.
        """

        info = await super().package_info(package)
        if not info.cask_known:
            async with self.lock:
                brew_result = await self.brew_info_cask(package)
                results = await self.fill_package_info(brew_result)
                info.cask = results[package].cask if package in results else None
                info.cask_known = True
        return info

    @classmethod
    async def fill_package_info(
        cls, run_result: RunResultOutput
    ) -> dict[str, BrewPackageVersionInfo]:
        results: dict[str, BrewPackageVersionInfo] = {}

        if not run_result.ok:
            return results
        info = json.loads(run_result.output)

        for cask in info["casks"]:
            if cask["tap"] == "homebrew/cask":
                # Do not require prefixes for casks from homebrew/cask
                name = cask["token"]
            else:
                name = f"{cask['tap']}/{cask['token']}"
            results[name] = BrewPackageVersionInfo(
                cask=PackageVersionInfo(
                    installed=cask["installed"], latest=cask["version"]
                ),
                cask_known=True,
                formula=None,
            )

        for formula in info["formulae"]:
            name = formula["name"]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            latest = cls.formula_version(formula)
            pvi = PackageVersionInfo(installed=installed, latest=latest)

            if installed and name in results:
                # Cask with the same name exists. If a _formula_ is installed
                # instead, record the formula version as installed too, so the
                # package will be updated to cask.
                results[name].formula = pvi
            else:
                results[name] = BrewPackageVersionInfo(
                    cask=None, cask_known=False, formula=pvi
                )

        return results

    @staticmethod
    def formula_version(info: dict) -> str:
        version = info["versions"]["stable"]
        revision = info.get("revision")
        if revision:
            version += f"_{revision}"
        return version


class DNF(PackageCacheInstaller[PackageVersionInfo]):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run("dnf", "install", "-y", package)
        await super().install(package)

    # can't install an old version of a package in tests
    async def upgrade(self, package: str) -> None:  # pragma: no cover
        await self.driver.with_root(True).run("dnf", "upgrade", "-y", package)
        await super().upgrade(package)

    @staticmethod
    def parse_versions(output: str, package: Optional[str]) -> dict[str, str]:
        versions = {}
        for line in output.splitlines():
            name, version = line.split()
            versions[name] = version

        if package is not None:
            if package in versions:
                return {package: versions[package]}
            # If querying for a specific package, the name might be different
            # because of virtual packages and --whatprovides option.
            # If there's no package with exact name, require a single match.
            if len(versions) > 1:
                raise ValueError(
                    f"Multiple versions for {package}: {versions}."
                )  # pragma: no cover
            if len(versions) == 0:
                raise ValueError(f"No versions for {package}.")
            (version,) = versions.values()  # pylint:disable=unbalanced-dict-unpacking
            return {package: version}

        return versions

    async def rpm_query(self, package: Optional[str]) -> dict[str, str]:
        """
        Query RPM for the installed package versions.

        @param package: If specified, only query this package; otherwise,
        return all packages' versions.
        """
        args = [
            "rpm",
            "--query",
            "--queryformat",
            "%{NAME} %{VERSION}\n",
        ]

        if package:
            args += ["--whatprovides", package]
        else:
            args += ["--all"]

        result = await self.driver.run_output_(*args)
        if not result.ok:
            return {}
        return self.parse_versions(result.output, package)

    async def dnf_repoquery(self, package: Optional[str]) -> dict[str, str]:
        """
        Query DNF for the latest package versions.

        @param package: If specified, only query this package; otherwise,
        return all packages' versions.
        """

        arch = await self.driver.architecture()

        args = [
            "dnf",
            "--quiet",
            "repoquery",
            "--queryformat",
            "%{name} %{version}",
            "--latest-limit",
            "1",
            "--arch",
            f"{arch},noarch",
        ]

        if package:
            args += ["--whatprovides", package]

        output = await self.driver.run_output(*args)
        return self.parse_versions(output, package)

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, PackageVersionInfo]:
        latest = await self.dnf_repoquery(package)
        installed = await self.rpm_query(package)

        return {
            name: PackageVersionInfo(
                installed=installed.get(name), latest=latest_version
            )
            for name, latest_version in latest.items()
        }


class Apt(Installer):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run(
            "env", "DEBIAN_FRONTEND=noninteractive", "apt", "install", "--yes", package
        )

    async def upgrade(self, package: str) -> None:
        await self.install(package)

    async def latest_version(self, package: str) -> str:
        result = await self.driver.run_output_(
            "apt-cache",
            "show",
            "--quiet",
            "--no-all-versions",
            package,
        )
        if not result.ok:
            raise ValueError(f"Cannot determine version for: {package}.")
        for line in result.output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot parse apt output: {result.output}")  # pragma: no cover

    async def installed_version(self, package: str) -> Optional[str]:
        return (
            await self.driver.run_output_(
                "dpkg-query", "--showformat", "${Version}", "--show", package
            )
        ).output


async def linux_installer(driver: Driver) -> Installer:
    if await driver.executable_exists("dnf"):
        return DNF(driver)
    elif await driver.executable_exists("apt"):
        return Apt(driver)
    else:
        raise NotImplementedError("Cannot find a package manager.")


async def macos_installer(driver: Driver) -> Installer:
    return Brew(driver)


@async_cached_lock
async def make_installer(driver: Driver) -> Installer:
    os = await driver.os()
    installer_fn = os.switch(linux=linux_installer, macos=macos_installer)
    return await installer_fn(driver)
