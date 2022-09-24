import os
import random
from abc import ABCMeta, abstractmethod
from functools import cached_property
from pathlib import Path
from typing import Any, Iterable, Optional

import pytest

from mybox.driver import Driver
from mybox.package import Package, parse_package
from mybox.state import DB

from .driver import DockerDriver, OverrideHomeDriver, RootCheckDriver

PackageArgs = dict[str, Any]


class PackageTestBase(metaclass=ABCMeta):
    db: DB
    driver: RootCheckDriver

    @property
    @abstractmethod
    def constructor_args(self) -> PackageArgs:
        pass

    @property
    @abstractmethod
    def check_installed_command(self) -> Iterable[str]:
        pass

    check_installed_output: Optional[str] = None

    @property
    def prerequisites(self) -> Iterable[PackageArgs]:
        return []

    @pytest.fixture(autouse=True)
    def setup_driver(self, monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
        docker_image = os.environ.get("DOCKER_IMAGE")
        if docker_image:
            self.driver = DockerDriver.create(image=docker_image)
        else:
            local_bin = tmp_path / ".local" / "bin"
            monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")
            self.driver = OverrideHomeDriver(override_home=tmp_path)

    @pytest.fixture(autouse=True)
    def setup_db(self) -> None:
        self.db = DB(":memory:")

    def parse_package(
        self, constructor_args: PackageArgs, driver: Optional[Driver] = None
    ) -> Package:
        return parse_package(constructor_args, db=self.db, driver=driver or self.driver)

    @property
    def test_driver(self) -> Driver:
        return self.driver

    def check_installed(self):
        if self.check_installed_output is None:
            self.test_driver.run_ok(*self.check_installed_command)
        else:
            output = self.test_driver.run_output(*self.check_installed_command)
            assert self.check_installed_output in output

    def test_installs(self):
        for prerequisite in self.prerequisites:
            package = self.parse_package(prerequisite)
            package.ensure()

        package = self.parse_package(self.constructor_args)
        assert package.applicable
        package.install()
        self.check_installed()

    root_required_for_is_installed = False

    def test_no_root_required_for_is_installed(self):
        if self.root_required_for_is_installed:
            return

        package = self.parse_package(
            self.constructor_args, driver=self.driver.disable_root()
        )
        assert not package.is_installed

    JAVA: list[PackageArgs] = [
        {"name": "java-17-openjdk", "os": "linux", "distribution": "fedora"},
        {"name": "openjdk-17-jre", "os": "linux", "distribution": ["debian", "ubuntu"]},
    ]

    NODE: list[PackageArgs] = [{"name": "nodejs", "os": "linux"}]


class DestinationPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    @cached_property
    def dir_name(self) -> str:
        return f"mybox_test_{random.randint(0, 1000000)}"

    @property
    def destination(self) -> Path:
        return self.test_driver.home() / self.dir_name

    def test_installs(self):
        try:
            return super().test_installs()
        finally:
            self.test_driver.run("rm", "-rf", str(self.destination))


class RootPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    root = False

    @property
    def test_driver(self) -> Driver:
        return super().test_driver.with_root(self.root)
