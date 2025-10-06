from collections.abc import Sequence
from typing import Any, Union

from pydantic import TypeAdapter

from ..driver import Driver
from ..state import DB
from .base import Package, PackageArgs
from .clone import Clone
from .daemon import Daemon
from .links import Links
from .npm import NpmPackage
from .system import SystemPackage

AnyPackageT = Union[
    Clone,
    Daemon,
    Links,
    NpmPackage,
    SystemPackage,
]

AnyPackage: TypeAdapter[AnyPackageT] = TypeAdapter(AnyPackageT)

AnyPackageList = TypeAdapter(list[AnyPackageT])


def parse_package(package: Any, *, db: DB, driver: Driver) -> Package:
    package = TypeAdapter(dict).validate_python(package)

    return AnyPackage.validate_python({**package, "db": db, "driver": driver})


def parse_packages(packages: Any, *, db: DB, driver: Driver) -> Sequence[Package]:
    packages = TypeAdapter(list[dict]).validate_python(packages)

    return AnyPackageList.validate_python(
        [{**package, "db": db, "driver": driver} for package in packages]
    )
