import json
import shutil
import subprocess
from abc import ABCMeta, abstractmethod
from functools import cache
from threading import Lock
from typing import Optional

from ..state import Version
from ..utils import *
from .base import Package


class Installer(metaclass=ABCMeta):
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
        run("brew", "install", package)

    def upgrade(self, package: str) -> None:
        run("brew", "upgrade", package)

    @cache
    def info(self, package: str) -> tuple[str, Optional[str]]:
        info = json.loads(run_output("brew", "info", "--json=v2", package))
        if info["casks"]:
            cask = info["casks"][0]
            return cask["version"], cask["installed"]
        if info["formulae"]:
            formula = info["formulae"][0]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            version = formula["versions"]["stable"]
            revision = formula.get("revision")
            if revision:
                version += f"_{revision}"
            return version, installed
        raise ValueError(f"Unexpected output from brew: {info}")

    def installed_version(self, package: str) -> Optional[str]:
        try:
            return self.info(package)[1]
        except IndexError:
            return None

    def latest_version(self, package: str) -> str:
        return self.info(package)[0]


class DNF(Installer):
    def install(self, package: str) -> None:
        run("sudo", "dnf", "install", "-y", package)

    def upgrade(self, package: str) -> None:
        run("sudo", "dnf", "upgrade", "-y", package)

    def installed_version(self, package: str) -> Optional[str]:
        try:
            output = run_output(
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
        output = run_output(
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
        run("sudo", "apt", "install", "--yes", package)

    def upgrade(self, package: str) -> None:
        self.install(package)

    def latest_version(self, package: str) -> str:
        output = run_output(
            "apt-cache", "show", "--quiet", "--no-all-versions", package
        ).strip()
        for line in output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot determine version for {package}.")

    def installed_version(self, package: str) -> Optional[str]:
        try:
            return run_output(
                "dpkg-query",
                "--showformat",
                "${Version}",
                "--show",
                package,
                stderr=subprocess.DEVNULL,
            ).strip()
        except subprocess.CalledProcessError:
            return None


def linux_installer() -> Installer:
    if shutil.which("dnf"):
        return DNF()
    elif shutil.which("apt"):
        return Apt()
    else:
        raise NotImplementedError("Cannot find a package manager.")


INSTALLER: Installer = with_os(
    linux=linux_installer, macos=lambda: Brew()  # pylint:disable=unnecessary-lambda
)()

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

    @property
    def name(self) -> str:
        return self._name

    def get_remote_version(self) -> str:
        if self.url:
            return url_version(self.url)
        if self.auto_updates:
            return "latest"
        return INSTALLER.latest_version(self.name)

    @property
    def local_version(self) -> Optional[str]:
        if self.url:
            try:
                return self.versions[self.name].version
            except KeyError:
                return None
        if self.auto_updates:
            return "latest" if INSTALLER.is_installed(self.name) else None
        return INSTALLER.installed_version(self.name)

    def postinstall_linux(self):
        if self.services:
            run("sudo", "systemctl", "daemon-reload")
            for service in self.services:
                run("sudo", "systemctl", "enable", service)

    def postinstall_macos(self):
        pass

    def install(self):
        with INSTALLER_LOCK:
            if self.url:
                INSTALLER.install(self.url)
                self.versions[self.name] = Version(version=self.get_remote_version())
            elif self.local_version:
                INSTALLER.upgrade(self.name)
            else:
                INSTALLER.install(self.name)
        with_os(linux=self.postinstall_linux, macos=self.postinstall_macos)()
