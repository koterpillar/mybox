from collections.abc import Sequence
from typing import Any, Union

from pydantic import TypeAdapter

from ..driver import Driver
from ..state import DB
from .base import Package
from .brew_repo import BrewRepo
from .clone import Clone
from .daemon import Daemon
from .github import GitHubPackage
from .links import Links
from .npm import NpmPackage
from .pipx import PipxPackage
from .shell import Shell
from .system import SystemPackage
from .url import URLPackage
from .yum_repo import YumRepo

AnyPackageT = Union[
    BrewRepo,
    Clone,
    Daemon,
    GitHubPackage,
    Links,
    NpmPackage,
    PipxPackage,
    Shell,
    SystemPackage,
    URLPackage,
    YumRepo,
]

AnyPackage: TypeAdapter[AnyPackageT] = TypeAdapter(AnyPackageT)

AnyPackageList = TypeAdapter(list[AnyPackageT])


def parse_package(package: Any, *, db: DB, driver: Driver) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected, got: {package}.")

    return AnyPackage.validate_python({**package, "db": db, "driver": driver})


def parse_packages(packages: Any, *, db: DB, driver: Driver) -> Sequence[Package]:
    if not isinstance(packages, list):
        raise ValueError(f"List expected, got: {packages}.")

    return AnyPackageList.validate_python(
        [{**package, "db": db, "driver": driver} for package in packages]
    )
