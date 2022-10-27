import json
from abc import ABCMeta, abstractmethod
from asyncio import Lock
from functools import cache
from typing import Optional

import requests

from ..driver import Driver
from ..utils import Some, async_cached, unsome, url_version
from .manual_version import ManualVersion


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

    async def is_installed(self, package: str) -> bool:
        return self.installed_version(package) is not None

    @abstractmethod
    async def installed_version(self, package: str) -> Optional[str]:
        pass

    @abstractmethod
    async def latest_version(self, package: str) -> str:
        pass


class Brew(Installer):
    async def install(self, package: str) -> None:
        await self.driver.run("brew", "install", package)

    async def upgrade(self, package: str) -> None:
        await self.driver.run("brew", "upgrade", package)

    @cache
    async def brew_info(self, package: str) -> dict:
        return json.loads(
            await self.driver.run_output("brew", "info", "--json=v2", package)
        )

    async def installed_version(self, package: str) -> Optional[str]:
        info = await self.brew_info(package)
        if info["casks"]:
            cask = info["casks"][0]
            return cask["installed"]
        if info["formulae"]:
            formula = info["formulae"][0]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            return installed
        raise ValueError(f"Unexpected output from brew: {info}")

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
            if info.get("formulae"):
                formula = info["formulae"][0]
                return self.formula_version(formula)
            elif info.get("casks"):
                cask = info["casks"][0]
                return cask["version"]
            else:
                raise ValueError(f"Unknown package: {package}")

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
        return (
            await self.driver.run_(
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
        ).output

    async def latest_version(self, package: str) -> str:
        output = await self.driver.run_output(
            "dnf",
            "--quiet",
            "repoquery",
            "--queryformat",
            "%{VERSION}",
            "--latest-limit",
            "1",
            "--arch",
            "x86_64,noarch",
            "--whatprovides",
            package,
        )
        if not output or "\n" in output:
            raise Exception(f"Cannot determine version for {package}.")
        return output


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


INSTALLER_LOCK = Lock()


class SystemPackage(ManualVersion):
    def __init__(
        self,
        *,
        name: str,
        url: Optional[str] = None,
        auto_updates: bool = False,
        service: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self._name = name
        self.url = url
        self.auto_updates = auto_updates
        self.services = unsome(service)

    @async_cached
    async def installer(self):
        return (await self.driver.os()).switch(
            linux=await linux_installer(self.driver), macos=Brew(self.driver)
        )

    @property
    def name(self) -> str:
        return self._name

    async def get_remote_version(self) -> str:
        if self.url:
            return url_version(self.url)
        if self.auto_updates:
            return "latest"
        return await (await self.installer()).latest_version(self.name)

    async def local_version(self) -> Optional[str]:
        if self.url:
            return self.cached_version
        installer = await self.installer()
        if self.auto_updates:
            return "latest" if await installer.is_installed(self.name) else None
        return await installer.installed_version(self.name)

    async def postinstall_linux(self):
        if self.services:
            await self.driver.with_root(True).run("systemctl", "daemon-reload")
            for service in self.services:
                await self.driver.with_root(True).run("systemctl", "enable", service)

    async def postinstall_macos(self):
        pass

    async def install(self):
        async with INSTALLER_LOCK:
            installer = await self.installer()
            if self.url:
                await installer.install(self.url)
                await self.cache_version()
            elif await self.local_version():
                await installer.upgrade(self.name)
            else:
                await installer.install(self.name)
        await (await self.driver.os()).switch(
            linux=self.postinstall_linux, macos=self.postinstall_macos
        )()
