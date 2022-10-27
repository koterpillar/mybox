import os
import random
from abc import ABCMeta, abstractmethod
from functools import cached_property, wraps
from pathlib import Path
from typing import Any, Callable, Coroutine, Iterable, Optional, Union

import pytest

from mybox.driver import OS, LocalDriver
from mybox.package import Package, parse_package
from mybox.state import DB
from mybox.utils import T

from .driver import DockerDriver, Driver, OverrideHomeDriver, RootCheckDriver

PackageArgs = dict[str, Union[str, bool, int, Path, list[str]]]


CI: bool = "CI" in os.environ


def requires_driver(
    test_fn: Callable[[T, RootCheckDriver], Coroutine[Any, Any, None]]
) -> Callable[[T, RootCheckDriver], Coroutine[Any, Any, None]]:
    @wraps(test_fn)
    async def wrapper(self, make_driver):
        self.driver = make_driver
        return await test_fn(self, make_driver)

    return wrapper


class PackageTestBase(metaclass=ABCMeta):
    db: DB
    driver: RootCheckDriver

    @abstractmethod
    async def constructor_args(self) -> PackageArgs:
        pass

    @abstractmethod
    async def check_installed_command(self) -> Iterable[Union[str, Path]]:
        pass

    check_installed_output: Optional[str] = None

    @property
    def prerequisites(self) -> Iterable[PackageArgs]:
        return []

    affects_system = False  # If True, local tests won't run unless in Docker

    @pytest.fixture
    async def make_driver(
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
        await self.check_applicable()
        return driver

    async def check_applicable(self) -> None:
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

    @property
    def check_driver(self) -> Driver:
        return self.driver

    async def check_installed(self):
        command = await self.check_installed_command()
        if self.check_installed_output is None:
            await self.check_driver.run_ok(*command)
        else:
            output = await self.check_driver.run_output(*command)
            assert self.check_installed_output in output

    @pytest.mark.trio
    @requires_driver
    async def test_installs(
        self, make_driver: RootCheckDriver  # pylint:disable=unused-argument
    ):
        for prerequisite in self.prerequisites:
            package = self.parse_package(prerequisite, driver=self.driver)
            await package.ensure()

        db = self.setup_db()

        args = await self.constructor_args()

        package = self.parse_package(args, driver=self.driver, db=db)
        assert await package.applicable()

        await package.install()
        await self.check_installed()

        # Create the package again to reset cached properties
        package = self.parse_package(args, driver=self.driver, db=db)
        assert await package.is_installed()

    root_required_for_is_installed = False

    @pytest.mark.trio
    @requires_driver
    async def test_no_root_required_for_is_installed(
        self, make_driver: RootCheckDriver  # pylint:disable=unused-argument
    ):
        if self.root_required_for_is_installed:
            return

        args = await self.constructor_args()

        package = self.parse_package(args, driver=self.driver.disable_root())
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

    async def destination(self) -> Path:
        return (await self.check_driver.home()) / self.dir_name

    @pytest.mark.trio
    @requires_driver
    async def test_installs(self, make_driver: RootCheckDriver):
        try:
            return await super().test_installs(make_driver)
        finally:
            await self.check_driver.run("rm", "-rf", await self.destination())


class RootPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    root = False

    @property
    def check_driver(self) -> Driver:
        return super().check_driver.with_root(self.root)
