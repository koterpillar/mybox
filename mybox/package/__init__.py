from collections.abc import Sequence
from typing import Any, Union

from pydantic import TypeAdapter

from ..driver import Driver
from ..state import DB
from .base import Package, PackageArgs
from .brew_repo import BrewRepo
from .clone import Clone
from .daemon import Daemon
from .flatpak import FlatpakPackage
from .links import Links
from .npm import NpmPackage
from .shell import Shell
from .system import SystemPackage
from .yum_repo import YumRepo

AnyPackageT = Union[
    BrewRepo,
    Clone,
    Daemon,
    FlatpakPackage,
    Links,
    NpmPackage,
    Shell,
    SystemPackage,
    YumRepo,
]

AnyPackage: TypeAdapter[AnyPackageT] = TypeAdapter(AnyPackageT)

AnyPackageList = TypeAdapter(list[AnyPackageT])


def parse_package(package: Any, *, db: DB, driver: Driver) -> Package:
    package = TypeAdapter(dict).validate_python(package)

    return AnyPackage.validate_python({**package, "db": db, "driver": driver})


def keep_package(package: dict) -> bool:
    implementation = package.pop("$implementation", None)
    if implementation and implementation != "python":
        return False

    if any(k in package for k in ("repo", "pipx")):
        return False

    if "url" in package and "system" not in package:
        return False

    return True


def parse_packages(packages: Any, *, db: DB, driver: Driver) -> Sequence[Package]:
    packages = TypeAdapter(list[dict]).validate_python(packages)

    packages = list(filter(keep_package, packages))

    return AnyPackageList.validate_python(
        [{**package, "db": db, "driver": driver} for package in packages]
    )
