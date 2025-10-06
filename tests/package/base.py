from abc import ABC, abstractmethod
from collections.abc import Iterable
from pathlib import Path

import pytest

from mybox.manager import Manager
from mybox.package import PackageArgs, parse_package, parse_packages
from mybox.state import DB
from mybox.utils import RunArg

from ..base import CI, DOCKER
from .driver import TestDriver


class PackageTestBase(ABC):
    db: DB
    driver: TestDriver

    @abstractmethod
    async def constructor_args(self) -> PackageArgs:
        pass

    @abstractmethod
    async def check_installed_command(self) -> Iterable[RunArg]:
        pass

    check_installed_output: str

    @property
    def prerequisites(self) -> Iterable[PackageArgs]:
        return []

    async def check_applicable(self) -> None:
        if not DOCKER and not CI:
            pytest.skip("Test affects local system.")

    def setup_db(self) -> DB:
        return DB.temporary()

    async def check_installed(self):
        command = await self.check_installed_command()
        output = await self.driver.run_output(*command)
        assert self.check_installed_output in output

    async def install_prerequisites(self) -> None:
        db = self.setup_db()
        manager = Manager(db=db, driver=self.driver, data_path=Path("/dev/null"))
        packages = list(parse_packages(self.prerequisites, db=db, driver=self.driver))
        result = await manager.install_packages(packages)
        result.raise_for_failures()

    @pytest.mark.trio
    async def test_installs(
        self, make_driver: TestDriver  # pylint:disable=unused-argument
    ):
        self.driver = make_driver
        await self.check_applicable()
        db = self.setup_db()

        args = await self.constructor_args()
        package = parse_package(args, driver=self.driver, db=db)

        assert await package.applicable()

        await self.install_prerequisites()

        await package.install()

        await self.check_installed()

        # Create the package again to reset cached properties
        package = parse_package(args, driver=self.driver, db=db)
        assert (
            await package.is_installed()
        ), "Package should be reported installed after installation."
        assert (await package.local_version()) == (
            await package.get_remote_version()
        ), "Package version should be the same as the remote version."
