import os
import random
from abc import ABCMeta, abstractmethod
from functools import cached_property
from pathlib import Path
from typing import Iterable, Optional, Union

import pytest

from mybox.driver import OS, LocalDriver
from mybox.package import Package, parse_package
from mybox.state import DB

from .driver import DockerDriver, Driver, OverrideHomeDriver, RootCheckDriver

PackageArgs = dict[str, Union[str, bool, int, Path, list[str]]]


CI: bool = "CI" in os.environ


class PackageTestBase(metaclass=ABCMeta):
    db: DB

    @abstractmethod
    async def constructor_args(self, driver: RootCheckDriver) -> PackageArgs:
        pass

    @abstractmethod
    async def check_installed_command(
        self, driver: RootCheckDriver
    ) -> Iterable[Union[str, Path]]:
        pass

    check_installed_output: Optional[str] = None

    @property
    def prerequisites(self) -> Iterable[PackageArgs]:
        return []

    affects_system = False  # If True, local tests won't run unless in Docker

    @pytest.fixture
    async def driver(
        self, monkeypatch: pytest.MonkeyPatch, tmp_path: Path
    ) -> RootCheckDriver:
        driver: RootCheckDriver
        docker_image = os.environ.get("DOCKER_IMAGE")
        if docker_image:
            driver = DockerDriver.create(image=docker_image)
        else:
            if self.affects_system and not CI:
                pytest.skip("Skipping test on local machine")
            local_bin = tmp_path / ".local" / "bin"
            monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")
            driver = OverrideHomeDriver(override_home=tmp_path)
        await self.check_applicable(driver)
        return driver

    async def check_applicable(self, driver: RootCheckDriver) -> None:
        pass

    def setup_db(self) -> DB:
        return DB(":memory:")

    def parse_package(
        self,
        constructor_args: PackageArgs,
        *,
        driver: Driver,
        db: Optional[DB] = None,
    ) -> Package:
        return parse_package(constructor_args, db=db or self.setup_db(), driver=driver)

    def check_driver(self, driver: RootCheckDriver) -> Driver:
        return driver

    async def check_installed(self, driver: RootCheckDriver):
        command = await self.check_installed_command(driver)
        if self.check_installed_output is None:
            await self.check_driver(driver).run_ok(*command)
        else:
            output = await self.check_driver(driver).run_output(*command)
            assert self.check_installed_output in output

    @pytest.mark.trio
    async def test_installs(self, driver: RootCheckDriver):
        for prerequisite in self.prerequisites:
            package = self.parse_package(prerequisite, driver=driver)
            await package.ensure()

        db = self.setup_db()

        args = await self.constructor_args(driver)

        package = self.parse_package(args, driver=driver, db=db)
        assert await package.applicable()

        await package.install()
        await self.check_installed(driver)

        # Create the package again to reset cached properties
        package = self.parse_package(args, driver=driver, db=db)
        assert await package.is_installed()

    root_required_for_is_installed = False

    @pytest.mark.trio
    async def test_no_root_required_for_is_installed(self, driver: RootCheckDriver):
        if self.root_required_for_is_installed:
            return

        args = await self.constructor_args(driver)

        package = self.parse_package(args, driver=driver.disable_root())
        assert await package.applicable()
        # Check that the property works, installation status doesn't matter
        # (if running on the host machine system packages might or might not
        # be installed)
        assert await package.is_installed() in {True, False}

    JAVA: list[PackageArgs] = [
        {"name": "java-17-openjdk", "os": "linux", "distribution": "fedora"},
        {"name": "openjdk-17-jre", "os": "linux", "distribution": ["debian", "ubuntu"]},
    ]

    NODE: list[PackageArgs] = [{"name": "nodejs", "os": "linux"}]

    async def os(self) -> OS:
        return await LocalDriver().os()


class DestinationPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    @cached_property
    def dir_name(self) -> str:
        return f"mybox_test_{random.randint(0, 1000000)}"

    async def destination(self, driver: RootCheckDriver) -> Path:
        return (await self.check_driver(driver).home()) / self.dir_name

    @pytest.mark.trio
    async def test_installs(self, driver: RootCheckDriver):
        try:
            return await super().test_installs(driver)
        finally:
            await self.check_driver(driver).run(
                "rm", "-rf", await self.destination(driver)
            )


class RootPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    root = False

    def check_driver(self, driver: RootCheckDriver) -> Driver:
        return super().check_driver(driver).with_root(self.root)
