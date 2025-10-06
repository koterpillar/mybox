from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

from .config import MatchConfig, parse_config
from .driver import Driver
from .package import Package, parse_packages
from .parallel import PartialException, PartialResults
from .state import DB


@dataclass
class InstallResult:
    installed: list[Package]
    failed: list[tuple[Package, BaseException]]

    def raise_for_failures(self) -> None:  # pragma: no cover
        if self.failed:
            for _, exc in self.failed:
                raise exc


@dataclass
class Manager:
    db: DB
    driver: Driver
    data_path: Path

    def load_data(self, *path: str) -> Any:
        with open(self.data_path / Path(*path)) as f:
            return yaml.safe_load(f)

    def load_config(self) -> list[MatchConfig]:
        return parse_config(self.load_data("mybox.yaml"), driver=self.driver)

    def load_component(self, component: str) -> Sequence[Package]:
        packages = self.load_data("packages", f"{component}.yaml")
        return parse_packages(packages, db=self.db, driver=self.driver)

    def load_components(self, components: frozenset[str]) -> list[Package]:
        return [
            pkg for component in components for pkg in self.load_component(component)
        ]

    async def install(self) -> InstallResult:
        config = self.load_config()
        components = await MatchConfig.components(config)
        packages = self.load_components(frozenset(components))

        return await self.install_packages(packages)

    async def install_packages(self, packages: list[Package]) -> InstallResult:
        try:
            for pkg in packages:
                if not await pkg.applicable():
                    continue

                if await pkg.is_installed():
                    continue

                await pkg.install()

            return InstallResult(installed=packages, failed=[])
        except PartialResults as e:
            installed: list[Package] = []
            failed: list[tuple[Package, BaseException]] = []

            for package, result in zip(packages, e.results):
                if isinstance(result, PartialException):
                    failed.append((package, result.exception))
                else:
                    installed.extend(result.result)

            return InstallResult(installed=installed, failed=failed)
