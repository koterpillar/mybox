from typing import Any, Type

from ..driver import Driver
from ..state import DB
from .base import Package
from .brew_repo import BrewRepo
from .clone import Clone
from .github import GitHubPackage
from .links import Links
from .npm import NpmPackage
from .pipx import PipxPackage
from .shell import Shell
from .system import SystemPackage
from .url import URLPackage
from .yum_repo import YumRepo

TYPES: list[tuple[str, Type[Package]]] = [
    ("system", SystemPackage),
    ("repo", GitHubPackage),
    ("url", URLPackage),
    ("clone", Clone),
    ("npm", NpmPackage),
    ("pipx", PipxPackage),
    ("shell", Shell),
    ("links", Links),
    ("yum_name", YumRepo),
    ("brew_tap", BrewRepo),
]


def parse_package(package: Any, *, db: DB, driver: Driver) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected, got: {package}.")

    for key, package_type in TYPES:
        if key in package:
            return package_type(**package, db=db, driver=driver)

    keys = ", ".join(f"'{key}'" for key, _ in TYPES)
    raise ValueError(f"Either of {keys} must be present, got: {package}.")
