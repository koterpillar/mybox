import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Optional, cast

import trio

from ..driver import Driver
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


@dataclass
class PackageVersionInfo:
    installed: Optional[str]
    latest: str


class PackageCacheInstaller(Installer, metaclass=ABCMeta):
    cache: Optional[dict[str, PackageVersionInfo]]

    def __init__(self, driver: Driver) -> None:
        self.cache = None
        self.lock = trio.Lock()
        super().__init__(driver)

    @abstractmethod
    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, PackageVersionInfo]:
        pass

    async def package_info(self, package: str) -> PackageVersionInfo:
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


class Brew(PackageCacheInstaller):
    async def install(self, package: str) -> None:
        await self.driver.run(await self.brew(), "install", package)
        await super().install(package)

    async def upgrade(self, package: str) -> None:  # pragma: no cover
        # Can't install an old version of a package in tests
        await self.driver.run(await self.brew(), "upgrade", package)
        await super().upgrade(package)

    async def tap(self, repo: str) -> None:
        await self.driver.run(await self.brew(), "tap", repo)

    async def tapped(self) -> set[str]:
        output = await self.driver.run_output(await self.brew(), "tap")
        return set(output.strip().splitlines())

    @async_cached
    async def brew(self) -> str:
        # https://docs.brew.sh/FAQ#why-is-the-default-installation-prefix-opthomebrew-on-apple-silicon
        return await self.driver.find_executable("brew", "/opt/homebrew/bin/brew")

    @async_cached_lock
    async def brew_update(self) -> None:
        await self.driver.run(await self.brew(), "update")

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, PackageVersionInfo]:
        await self.brew_update()

        args = [await self.brew(), "info", "--json=v2"]
        if package:
            args.append(package)
        else:
            args.append("--installed")

        info = json.loads(await self.driver.run_output(*args))

        result = {}

        for cask in info["casks"]:
            name = f"{cask['tap']}/{cask['token']}"
            result[name] = PackageVersionInfo(
                installed=cask["installed"], latest=cask["version"]
            )

        for formula in info["formulae"]:
            name = formula["name"]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            result[name] = PackageVersionInfo(
                installed=installed, latest=self.formula_version(formula)
            )

        return result

    @staticmethod
    def formula_version(info: dict) -> str:
        version = info["versions"]["stable"]
        revision = info.get("revision")
        if revision:
            version += f"_{revision}"
        return version


class DNF(PackageCacheInstaller):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run("dnf", "install", "-y", package)
        await super().install(package)

    async def upgrade(self, package: str) -> None:
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
                raise ValueError(f"Multiple versions for {package}: {versions}.")
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

        check = await self.driver.run_(
            *args, check=False, silent=True, capture_output=True
        )
        if not check.ok:
            return {}
        output = cast(str, check.output)
        return self.parse_versions(output, package)

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
        check = await self.driver.run_(
            "apt-cache",
            "show",
            "--quiet",
            "--no-all-versions",
            package,
            check=False,
            silent=True,
            capture_output=True,
        )
        if not check.ok:
            raise ValueError(f"Cannot determine version for: {package}.")
        output = cast(str, check.output)
        for line in output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot parse apt output: {output}")  # pragma: no cover

    async def installed_version(self, package: str) -> Optional[str]:
        return (
            await self.driver.run_(
                "dpkg-query",
                "--showformat",
                "${Version}",
                "--show",
                package,
                check=False,
                silent=True,
                capture_output=True,
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
