from dataclasses import dataclass
from pathlib import Path
from typing import AsyncIterable, Iterable

import yaml

from .driver import Driver
from .package import Package, parse_package
from .parallel import PartialException, PartialResults, parallel_map_tqdm
from .state import DB, VERSIONS
from .tracker import ManagerTracker, Tracker
from .utils import flatten


@dataclass
class InstallResult:
    installed: list[Package]
    failed: list[tuple[Package, BaseException]]


@dataclass
class Manager:
    db: DB
    driver: Driver
    component_path: Path

    def load_component(self, component: str) -> list[Package]:
        with open(self.component_path / f"{component}.yaml") as f:
            packages = yaml.safe_load(f)
            return [
                parse_package(package, db=self.db, driver=self.driver)
                for package in packages
            ]

    def load_components(self, components: frozenset[str]) -> list[Package]:
        return flatten(self.load_component(component) for component in components)

    async def cleanup(self, packages: list[Package]) -> None:
        package_names = set(package.name for package in packages)

        versions = VERSIONS(self.db)

        all_versions = list(versions.find_ids())

        for package, _ in all_versions:
            if package not in package_names:
                versions.delete(id=package)

    async def install(self, components: frozenset[str]) -> InstallResult:
        packages = self.load_components(components)

        return await self.install_packages(packages)

    async def install_packages(self, packages: list[Package]) -> InstallResult:
        async with Tracker.tracking(driver=self.driver, db=self.db) as tracker:

            async def process_and_record(package: Package) -> Iterable[Package]:
                return [pkg async for pkg in self.install_package(tracker, package)]

            try:
                results = await parallel_map_tqdm(process_and_record, packages)

                await self.cleanup(packages)

                return InstallResult(installed=flatten(results), failed=[])
            except PartialResults as e:
                installed: list[Package] = []
                failed: list[tuple[Package, BaseException]] = []

                for package, result in zip(packages, e.results):
                    if isinstance(result, PartialException):
                        failed.append((package, result.exception))
                    else:
                        installed.extend(result.result)

                return InstallResult(installed=installed, failed=failed)

    async def install_package(
        self, tracker: ManagerTracker, package: Package
    ) -> AsyncIterable[Package]:
        async for prerequisite in package.prerequisites():
            async for result in self.install_package(tracker, prerequisite):
                yield result

        try:
            if not await package.applicable():
                return

            if await package.is_installed():
                tracker.skip(package.name)
                return

            await package.install(tracker=tracker.track(package.name))
        except:
            tracker.skip(package.name)
            raise

        yield package
