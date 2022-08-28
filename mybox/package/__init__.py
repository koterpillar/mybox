from pathlib import Path
from typing import Any, Tuple, Type

import yaml

from ..driver import Driver
from ..state import DB
from .base import Package
from .clone import Clone
from .github import GitHubPackage
from .links import Links
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


def load_packages(component: str, *, db: DB, driver: Driver) -> list[Package]:
    with open(Path("packages") / f"{component}.yaml") as f:
        packages = yaml.safe_load(f)
        return [parse_package(package, db=db, driver=driver) for package in packages]
