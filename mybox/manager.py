from pathlib import Path
from typing import Optional

import yaml

from .driver import Driver
from .package import Package, parse_package
from .state import DB, INSTALLED_FILES
from .utils import flatten, parallel_map_tqdm


class Manager:
    def __init__(self, *, db: DB, driver: Driver, component_path: Path):
        self.db = db
        self.driver = driver
        self.component_path = component_path

    def load_component(self, component: str) -> list[Package]:
        with open(self.component_path / f"{component}.yaml") as f:
            packages = yaml.safe_load(f)
            return [
                parse_package(package, db=self.db, driver=self.driver)
                for package in packages
            ]

    def load_components(self, components: frozenset[str]) -> list[Package]:
        return flatten(self.load_component(component) for component in components)

    @staticmethod
    async def process_and_record(package: Package) -> Optional[Package]:
        if await package.ensure():
            return package
        return None

    async def cleanup(self, packages: list[Package]) -> None:
        installed_files = INSTALLED_FILES(self.db)

        orphans = set(installed_files.find())
        for package in packages:
            orphans -= set(installed_files.find(package=package.name))

        for orphan in orphans:
            await self.driver.rm(orphan.path_)
            installed_files.delete(**orphan.__dict__)

    async def install(self, components: frozenset[str]) -> list[Package]:
        packages = self.load_components(components)

        return await self.install_packages(packages)

    async def install_packages(self, packages: list[Package]) -> list[Package]:
        results = await parallel_map_tqdm(self.process_and_record, packages)

        await self.cleanup(packages)

        return list(filter(None, results))
