from collections.abc import AsyncIterable, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

from .config import MatchConfig, parse_config
from .driver import Driver
from .package import Package, parse_packages
from .parallel import PartialException, PartialResults
from .state import DB, VERSIONS
from .utils import flatten


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
        return flatten(self.load_component(component) for component in components)

    async def cleanup(self, packages: list[Package]) -> None:
        package_names = set(package.name for package in packages)

        versions = VERSIONS(self.db)

        all_versions = list(versions.find_ids())

        for package, _ in all_versions:
            if package not in package_names:
                versions.delete(id=package)

    async def install(self) -> InstallResult:
        config = self.load_config()
        components = await MatchConfig.components(config)
        packages = self.load_components(frozenset(components))

        return await self.install_packages(packages)

    async def install_packages(self, packages: list[Package]) -> InstallResult:
        try:
            results = []
            for pkg in packages:
                async for package in self.install_package(pkg):
                    results.append(package)

            await self.cleanup(packages)

            return InstallResult(installed=results, failed=[])
        except PartialResults as e:
            installed: list[Package] = []
            failed: list[tuple[Package, BaseException]] = []

            for package, result in zip(packages, e.results):
                if isinstance(result, PartialException):
                    failed.append((package, result.exception))
                else:
                    installed.extend(result.result)

            return InstallResult(installed=installed, failed=failed)

    async def install_package(self, package: Package) -> AsyncIterable[Package]:
        if not await package.applicable():
            return

        if await package.is_installed():
            return

        await package.install()

        yield package
