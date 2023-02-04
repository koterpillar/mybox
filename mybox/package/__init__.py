from typing import Any, Tuple, Type

from ..driver import Driver
from ..state import DB
from .base import Package
from .clone import Clone
from .github import GitHubPackage
from .links import Links
from .npm import NpmPackage
from .pip import PipPackage
from .pipx import PipxPackage
from .shell import Shell
from .system import SystemPackage
from .url import URLPackage
from .yum_repo import YumRepo

TYPES: list[Tuple[str, Type[Package]]] = [
    ("name", SystemPackage),
    ("repo", GitHubPackage),
    ("url", URLPackage),
    ("clone", Clone),
    ("npm", NpmPackage),
    ("pip", PipPackage),
    ("pipx", PipxPackage),
    ("shell", Shell),
    ("links", Links),
    ("yum_name", YumRepo),
]


def parse_package(package: Any, *, db: DB, driver: Driver) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected, got: {package}.")

    for key, package_type in TYPES:
        if key in package:
            return package_type(**package, db=db, driver=driver)

    keys = ", ".join(f"'{key}'" for key, _ in TYPES)
    raise ValueError(f"Either of {keys} must be present, got: {package}.")
