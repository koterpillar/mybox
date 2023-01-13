from typing import Optional

from .driver import Driver
from .package import Package, load_packages
from .state import DB, INSTALLED_FILES
from .utils import flatten, parallel_map_tqdm


class Manager:
    def __init__(self, *, db: DB, driver: Driver):
        self.db = db
        self.driver = driver

    def load_component(self, component: str) -> list[Package]:
        return load_packages(component, db=self.db, driver=self.driver)

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
