from pathlib import Path
from typing import Optional

import yaml

from .driver import Driver
from .package import Package, parse_package
from .state import DB, INSTALLED_FILES, VERSIONS
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

        all_files = list(installed_file for installed_file in installed_files.find())

        package_names = set(package.name for package in packages)

        existing_package_paths = set(
            installed_file.path
            for installed_file in all_files
            if installed_file.package in package_names
        )

        for installed_file in all_files:
            if installed_file.package not in package_names:
                installed_files.delete(**installed_file.__dict__)
                if installed_file.path not in existing_package_paths:
                    await self.driver.rm(installed_file.path_)

        versions = VERSIONS(self.db)

        all_versions = list(versions.find_ids())

        for package, _ in all_versions:
            if package not in package_names:
                versions.delete(id=package)

    async def install(self, components: frozenset[str]) -> list[Package]:
        packages = self.load_components(components)

        return await self.install_packages(packages)

    async def install_packages(self, packages: list[Package]) -> list[Package]:
        results = await parallel_map_tqdm(self.process_and_record, packages)

        await self.cleanup(packages)

        return list(filter(None, results))
