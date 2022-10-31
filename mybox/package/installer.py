import json
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from typing import Optional

import requests

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
class BrewRecord:
    installed: Optional[str]
    latest: str


class Brew(Installer):
    async def install(self, package: str) -> None:
        await self.driver.run("brew", "install", package)

    async def upgrade(self, package: str) -> None:
        await self.driver.run("brew", "upgrade", package)

    @async_cached_lock
    async def brew_info(self, package: Optional[str]) -> dict[str, BrewRecord]:
        args = ["brew", "info", "--json=v2"]
        if package:
            args.append(package)
        else:
            args.append("--installed")

        info = json.loads(await self.driver.run_output(*args))

        result = {}

        for cask in info["casks"]:
            name = f"{cask['tap']}/{cask['token']}"
            result[name] = BrewRecord(
                installed=cask["installed"], latest=cask["version"]
            )

        for formula in info["formulae"]:
            name = formula["name"]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            result[name] = BrewRecord(
                installed=installed, latest=self.formula_version(formula)
            )

        return result

    async def installed_version(self, package: str) -> Optional[str]:
        info = await self.brew_info(None)
        try:
            return info[package].installed
        except KeyError:
            return None

    async def api(self, path: str) -> dict:
        result = requests.get(f"https://formulae.brew.sh/api/{path}")
        result.raise_for_status()
        return result.json()

    CASK_PREFIX = "homebrew/cask/"

    @async_cached
    async def latest_version(self, package: str) -> str:
        if package.startswith(self.CASK_PREFIX):
            # Cask
            # https://formulae.brew.sh/docs/api/#get-formula-metadata-for-a-cask-formula
            cask_package = package[len(self.CASK_PREFIX) :]
            cask = await self.api(f"cask/{cask_package}.json")
            return cask["version"]
        elif "/" not in package:
            # Normal formula
            # https://formulae.brew.sh/docs/api/#get-formula-metadata-for-a-core-formula
            formula = await self.api(f"formula/{package}.json")
            return self.formula_version(formula)
        else:
            # Non-core cask or formula?
            # https://github.com/Homebrew/discussions/discussions/3618
            info = await self.brew_info(package)
            try:
                return info[package].latest
            except KeyError:
                raise ValueError(f"Unknown package: {package}") from None

    @staticmethod
    def formula_version(info: dict) -> str:
        version = info["versions"]["stable"]
        revision = info.get("revision")
        if revision:
            version += f"_{revision}"
        return version


class DNF(Installer):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run("dnf", "install", "-y", package)

    async def upgrade(self, package: str) -> None:
        await self.driver.with_root(True).run("dnf", "upgrade", "-y", package)

    async def installed_version(self, package: str) -> Optional[str]:
        check = await self.driver.run_(
            "rpm",
            "--query",
            "--queryformat",
            "%{VERSION}",
            "--whatprovides",
            package,
            check=False,
            silent=True,
            capture_output=True,
        )
        return check.output

    @async_cached_lock
    async def dnf_repoquery(self, package: Optional[str]) -> dict[str, str]:
        """
        Query DNF for the versions of installed packages.

        @param package: If specified, only query this package; otherwise,
        return all packages' versions.
        """
        args = [
            "dnf",
            "--quiet",
            "repoquery",
            "--queryformat",
            "%{NAME} %{VERSION}",
            "--latest-limit",
            "1",
            "--arch",
            "x86_64,noarch",
        ]

        if package:
            args += [
                "--whatprovides",
                package,
            ]

        output = await self.driver.run_output(*args)

        versions = {}
        for line in output.splitlines():
            name, version = line.split()
            versions[name] = version
        return versions

    async def latest_version(self, package: str) -> str:
        all_versions = await self.dnf_repoquery(None)
        try:
            version = all_versions[package]
            return version
        except KeyError:
            pass
        # Virtual packages won't be returned in the full list of packages, query
        # them individually.
        versions = await self.dnf_repoquery(package)
        if len(versions) > 1:
            raise ValueError(f"Multiple versions for {package}: {versions}.")
        if len(versions) == 0:
            raise ValueError(f"No versions for {package}.")
        (version,) = versions.values()
        return version


class Apt(Installer):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run("apt", "install", "--yes", package)

    async def upgrade(self, package: str) -> None:
        await self.install(package)

    async def latest_version(self, package: str) -> str:
        output = (
            await self.driver.run_output(
                "apt-cache", "show", "--quiet", "--no-all-versions", package
            )
        ).strip()
        for line in output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot determine version for {package}.")

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
