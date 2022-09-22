import json
import shutil
import subprocess
from abc import ABCMeta, abstractmethod
from functools import cache, cached_property
from threading import Lock
from typing import Optional

import requests

from ..fs import FS
from ..state import Version
from ..utils import Some, unsome, url_version, with_os
from .base import Package


class Installer(metaclass=ABCMeta):
    def __init__(self, fs: FS) -> None:
        self.fs = fs
        super().__init__()

    @abstractmethod
    def install(self, package: str) -> None:
        pass

    @abstractmethod
    def upgrade(self, package: str) -> None:
        pass

    def is_installed(self, package: str) -> bool:
        return self.installed_version(package) is not None

    @abstractmethod
    def installed_version(self, package: str) -> Optional[str]:
        pass

    @abstractmethod
    def latest_version(self, package: str) -> str:
        pass


class Brew(Installer):
    def install(self, package: str) -> None:
        self.fs.run("brew", "install", package)

    def upgrade(self, package: str) -> None:
        self.fs.run("brew", "upgrade", package)

    @cache
    def brew_info(self, package: str) -> dict:
        return json.loads(self.fs.run_output("brew", "info", "--json=v2", package))

    def installed_version(self, package: str) -> Optional[str]:
        info = self.brew_info(package)
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

    def api(self, path: str) -> dict:
        result = requests.get(f"https://formulae.brew.sh/api/{path}")
        result.raise_for_status()
        return result.json()

    CASK_PREFIX = "homebrew/cask/"

    @cache
    def latest_version(self, package: str) -> str:
        if package.startswith(self.CASK_PREFIX):
            # Cask
            # https://formulae.brew.sh/docs/api/#get-formula-metadata-for-a-cask-formula
            cask_package = package[len(self.CASK_PREFIX) :]
            cask = self.api(f"cask/{cask_package}.json")
            return cask["version"]
        elif "/" not in package:
            # Normal formula
            # https://formulae.brew.sh/docs/api/#get-formula-metadata-for-a-core-formula
            formula = self.api(f"formula/{package}.json")
            return self.formula_version(formula)
        else:
            # Non-core cask or formula?
            # https://github.com/Homebrew/discussions/discussions/3618
            info = self.brew_info(package)
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
    def install(self, package: str) -> None:
        self.fs.with_root(True).run("dnf", "install", "-y", package)

    def upgrade(self, package: str) -> None:
        self.fs.with_root(True).run("dnf", "upgrade", "-y", package)

    def installed_version(self, package: str) -> Optional[str]:
        try:
            output = self.fs.run_output(
                "rpm",
                "--query",
                "--queryformat",
                "%{VERSION}",
                "--whatprovides",
                package,
                stderr=subprocess.DEVNULL,
            ).strip()
        except subprocess.CalledProcessError:
            return None
        if not output:
            return None
        return output

    def latest_version(self, package: str) -> str:
        output = self.fs.run_output(
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
        ).strip()
        if not output or "\n" in output:
            raise Exception(f"Cannot determine version for {package}.")
        return output


class Apt(Installer):
    def install(self, package: str) -> None:
        self.fs.with_root(True).run("apt", "install", "--yes", package)

    def upgrade(self, package: str) -> None:
        self.install(package)

    def latest_version(self, package: str) -> str:
        output = self.fs.run_output(
            "apt-cache", "show", "--quiet", "--no-all-versions", package
        ).strip()
        for line in output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot determine version for {package}.")

    def installed_version(self, package: str) -> Optional[str]:
        try:
            return self.fs.run_output(
                "dpkg-query",
                "--showformat",
                "${Version}",
                "--show",
                package,
                stderr=subprocess.DEVNULL,
            ).strip()
        except subprocess.CalledProcessError:
            return None


def linux_installer(fs: FS) -> Installer:
    if shutil.which("dnf"):
        return DNF(fs)
    elif shutil.which("apt"):
        return Apt(fs)
    else:
        raise NotImplementedError("Cannot find a package manager.")


INSTALLER_LOCK = Lock()


class SystemPackage(Package):
    def __init__(
        self,
        *,
        name: str,
        url: Optional[str] = None,
        auto_updates: bool = False,
        service: Some[str] = None,
        **kwargs,
    ) -> None:
        self._name = name
        self.url = url
        self.auto_updates = auto_updates
        self.services = unsome(service)
        super().__init__(**kwargs)

    @cached_property
    def installer(self):
        return with_os(linux=linux_installer(self.fs), macos=Brew(self.fs))

    @property
    def name(self) -> str:
        return self._name

    def get_remote_version(self) -> str:
        if self.url:
            return url_version(self.url)
        if self.auto_updates:
            return "latest"
        return self.installer.latest_version(self.name)

    @property
    def local_version(self) -> Optional[str]:
        if self.url:
            try:
                return self.versions[self.name].version
            except KeyError:
                return None
        if self.auto_updates:
            return "latest" if self.installer.is_installed(self.name) else None
        return self.installer.installed_version(self.name)

    def postinstall_linux(self):
        if self.services:
            self.fs.with_root(True).run("systemctl", "daemon-reload")
            for service in self.services:
                self.fs.with_root(True).run("systemctl", "enable", service)

    def postinstall_macos(self):
        pass

    def install(self):
        with INSTALLER_LOCK:
            if self.url:
                self.installer.install(self.url)
                self.versions[self.name] = Version(version=self.get_remote_version())
            elif self.local_version:
                self.installer.upgrade(self.name)
            else:
                self.installer.install(self.name)
        with_os(linux=self.postinstall_linux, macos=self.postinstall_macos)()
