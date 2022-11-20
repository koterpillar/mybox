import os
import random
from abc import ABCMeta, abstractmethod
from functools import cached_property, wraps
from pathlib import Path
from typing import AsyncIterator, Callable, Iterable, Optional, TypeVar, Union, overload

import pytest

from mybox.driver import OS, LocalDriver
from mybox.installed_files import INSTALLED_FILES, Tracker, track_files
from mybox.package import Package, parse_package
from mybox.state import DB
from mybox.utils import AsyncRet, RunArg, T, async_cached

from .driver import DockerDriver, Driver, OverrideHomeDriver, TestDriver

PackageArgs = dict[str, Union[str, bool, int, Path, list[str]]]


CI: bool = "CI" in os.environ

DOCKER_IMAGE: Optional[str] = os.environ.get("DOCKER_IMAGE") or None

DOCKER: bool = DOCKER_IMAGE is not None

TEST = TypeVar("TEST", bound="PackageTestBase")


@overload
def requires_driver(
    test_fn: Callable[[TEST, TestDriver], AsyncRet[None]]
) -> Callable[[TEST, TestDriver], AsyncRet[None]]:
    ...


@overload
def requires_driver(
    test_fn: Callable[[TEST, TestDriver, T], AsyncRet[None]]
) -> Callable[[TEST, TestDriver, T], AsyncRet[None]]:
    ...


def requires_driver(
    test_fn: Callable[..., AsyncRet[None]]
) -> Callable[..., AsyncRet[None]]:
    @wraps(test_fn)
    async def wrapper(self: TEST, make_driver: TestDriver, *args, **kwargs) -> None:
        self.driver = make_driver
        await self.check_applicable()
        return await test_fn(self, make_driver, *args, **kwargs)

    return wrapper


class PackageTestBase(metaclass=ABCMeta):
    db: DB
    driver: TestDriver

    @abstractmethod
    async def constructor_args(self) -> PackageArgs:
        pass

    @abstractmethod
    async def check_installed_command(self) -> Iterable[RunArg]:
        pass

    check_installed_output: Optional[str] = None

    @property
    def prerequisites(self) -> Iterable[PackageArgs]:
        return []

    affects_system = False  # If True, local tests won't run unless in Docker

    @pytest.fixture
    async def make_driver(
        self, monkeypatch: pytest.MonkeyPatch, tmp_path: Path
    ) -> AsyncIterator[TestDriver]:
        driver: TestDriver
        if DOCKER_IMAGE:
            driver = await DockerDriver.create(image=DOCKER_IMAGE)
        else:
            local_bin = tmp_path / ".local" / "bin"
            monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")
            driver = OverrideHomeDriver(override_home=tmp_path)
        yield driver
        await driver.stop()

    async def check_applicable(self) -> None:
        if self.affects_system and not DOCKER and not CI:
            pytest.skip("Test affects local system.")

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

    async def install_prerequisites(self, *, tracker: Tracker) -> None:
        for args in self.prerequisites:
            package = self.parse_package(args, driver=self.driver)
            await package.ensure(tracker=tracker)

    async def all_files(self) -> set[Path]:
        return {
            path
            for base_path in [await self.check_driver.home()]
            for path in await self.check_driver.find(
                base_path, mindepth=1, file_type="f"
            )
            if not any(
                path.is_relative_to(ignored) for ignored in await self.ignored_paths()
            )
        }

    async def ignored_paths(self) -> set[Path]:
        return set([await self.driver.home() / ".cache"])

    @pytest.mark.trio
    @requires_driver
    async def test_installs(
        self, make_driver: TestDriver  # pylint:disable=unused-argument
    ):
        db = self.setup_db()

        preexisting_files = await self.all_files()

        async with track_files(db=db, driver=self.driver) as tracker:
            await self.install_prerequisites(tracker=tracker)

            args = await self.constructor_args()

            package = self.parse_package(args, driver=self.driver, db=db)
            assert await package.applicable()

            await package.install(tracker=tracker)

        await self.check_installed()

        # Create the package again to reset cached properties
        package = self.parse_package(args, driver=self.driver, db=db)
        assert (
            await package.is_installed()
        ), "Package should be reported installed after installation."

        installed_files = list(f.path_ for f in INSTALLED_FILES(db).find())

        for existing in await self.all_files() - preexisting_files:
            if not any(
                existing.is_relative_to(installed) for installed in installed_files
            ):
                assert False, f"File {existing} was not tracked."

    root_required_for_is_installed = False

    @pytest.mark.trio
    @requires_driver
    async def test_no_root_required_for_is_installed(
        self, make_driver: TestDriver  # pylint:disable=unused-argument
    ):
        if self.root_required_for_is_installed:
            return

        async with track_files(db=self.setup_db(), driver=self.driver) as tracker:
            await self.install_prerequisites(tracker=tracker)

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

    PIPX: list[PackageArgs] = [{"pip": "pipx"}]

    @async_cached
    async def os(self) -> OS:
        return await LocalDriver().os()

    @async_cached
    async def local(self) -> Path:
        return await self.driver.local()

    async def assert_desktop_file_exists(
        self, file_name: str, *, name: str, executable: Optional[str] = None
    ) -> None:
        if not (await self.driver.os()).switch(linux=True, macos=False):
            return

        local = await self.local()

        desktop_file = await self.check_driver.run_output(
            "cat", local / "share" / "applications" / f"{file_name}.desktop"
        )
        assert f"Name={name}" in desktop_file
        if executable:
            assert f"Exec={executable}" in desktop_file

        icon = next(
            (
                line.split("=", 1)[1]
                for line in desktop_file.splitlines()
                if line.startswith("Icon=")
            ),
            None,
        )
        if icon:
            await self.assert_icon_exists(icon)

    async def assert_icon_exists(self, name: str) -> None:
        if not (await self.driver.os()).switch(linux=True, macos=False):
            return

        local = await self.local()
        icons = local / "share" / "icons"

        assert await self.check_driver.is_dir(
            icons
        ), f"Icon directory {icons} doesn't exist."

        for extension, resolution in [
            ("svg", "scalable"),
            *[("png", f"{size}x{size}") for size in [2**i for i in range(4, 9)]],
        ]:
            if await self.check_driver.is_file(
                icons / "hicolor" / resolution / "apps" / f"{name}.{extension}"
            ):
                return

        files = await self.check_driver.find(
            icons, file_type=["f", "l"]
        )  # pragma: no cover
        assert False, f"Icon '{name}' not found. Files in icons directory: {files}"


class DestinationPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    @cached_property
    def dir_name(self) -> str:
        return f"mybox_test_{random.randint(0, 1000000)}"

    async def destination(self) -> Path:
        return (await self.check_driver.home()) / self.dir_name

    @pytest.mark.trio
    @requires_driver
    async def test_installs(self, make_driver: TestDriver):
        try:
            return await super().test_installs(make_driver)
        finally:
            await self.check_driver.run("rm", "-rf", await self.destination())


class RootPackageTestBase(PackageTestBase, metaclass=ABCMeta):
    root = False

    @property
    def check_driver(self) -> Driver:
        return super().check_driver.with_root(self.root)

    @async_cached
    async def local(self) -> Path:
        if self.root:
            return Path("/usr/local")
        return await super().local()
