import os
from typing import Any, Tuple, Type

import yaml

from .base import Package
from .clone import Clone
from .github import GitHubPackage
from .pipx import PipxPackage
from .shell import Shell
from .system import SystemPackage
from .url import URLPackage

TYPES: list[Tuple[str, Type[Package]]] = [
    ("name", SystemPackage),
    ("repo", GitHubPackage),
    ("url", URLPackage),
    ("clone", Clone),
    ("pipx", PipxPackage),
    ("shell", Shell),
]


def parse_package(package: Any) -> Package:
    if not isinstance(package, dict):
        raise ValueError(f"Dictionary expected, got: {package}.")

    for key, package_type in TYPES:
        if key in package:
            return package_type(**package)

    keys = ", ".join(f"'{key}'" for key, _ in TYPES)
    raise ValueError(f"Either of {keys} must be present, got: {package}.")


def load_packages(component: str) -> list[Package]:
    with open(os.path.join("packages", f"{component}.yaml")) as f:
        packages = yaml.safe_load(f)
        return list(map(parse_package, packages))
