from abc import ABC, abstractmethod
from collections.abc import Awaitable, Callable, Iterable
from functools import wraps
from pathlib import Path
from typing import Optional, TypeVar, overload

import pytest

from mybox.manager import Manager
from mybox.package import Package, PackageArgs, parse_package, parse_packages
from mybox.state import DB
from mybox.utils import RunArg, T, U

from ..base import CI, DOCKER
from .driver import Driver, TestDriver

TEST = TypeVar("TEST", bound="PackageTestBase")


@overload
def requires_driver(
    test_fn: Callable[[TEST, TestDriver], Awaitable[T]],
) -> Callable[[TEST, TestDriver], Awaitable[T]]: ...


@overload
def requires_driver(
    test_fn: Callable[[TEST, TestDriver, T], Awaitable[U]],
) -> Callable[[TEST, TestDriver, T], Awaitable[U]]: ...


def requires_driver(
    test_fn: Callable[..., Awaitable[T]],
) -> Callable[..., Awaitable[T]]:
    @wraps(test_fn)
    async def wrapper(self: TEST, make_driver: TestDriver, *args, **kwargs) -> T:
        self.driver = make_driver
        await self.check_applicable()
        return await test_fn(self, make_driver, *args, **kwargs)

    return wrapper


class PackageTestBase(ABC):
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

    async def check_applicable(self) -> None:
        if self.affects_system and not DOCKER and not CI:
            pytest.skip("Test affects local system.")

    def setup_db(self) -> DB:
        return DB.temporary()

    root = False

    @property
    def check_driver(self) -> Driver:
        return self.driver.with_root(self.root)

    async def check_installed(self):
        command = await self.check_installed_command()
        if self.check_installed_output is None:
            await self.check_driver.run_ok(*command)  # pragma: no cover
        else:
            output = await self.check_driver.run_output(*command)
            assert self.check_installed_output in output

    async def install_prerequisites(self, package: Package) -> None:
        db = self.setup_db()
        manager = Manager(db=db, driver=self.driver, data_path=Path("/dev/null"))
        packages = list(parse_packages(self.prerequisites, db=db, driver=self.driver))
        async for prerequisite in package.prerequisites():
            packages.append(prerequisite)
        result = await manager.install_packages(packages)
        result.raise_for_failures()

    async def cleanup(self) -> None:
        pass

    @pytest.mark.trio
    @requires_driver
    async def test_installs(
        self, make_driver: TestDriver  # pylint:disable=unused-argument
    ):
        db = self.setup_db()

        args = await self.constructor_args()
        package = parse_package(args, driver=self.driver, db=db)

        assert await package.applicable()

        await self.install_prerequisites(package)

        try:
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
        finally:
            await self.cleanup()

    root_required_for_is_installed = False

    @pytest.mark.trio
    @requires_driver
    async def test_no_root_required_for_is_installed(
        self, make_driver: TestDriver  # pylint:disable=unused-argument
    ):
        if self.root_required_for_is_installed:
            return

        # Installing prerequisites might require root
        args = await self.constructor_args()
        package_pre = parse_package(args, driver=self.driver, db=self.setup_db())
        await self.install_prerequisites(package_pre)

        package = parse_package(
            args, driver=self.driver.disable_root(), db=self.setup_db()
        )
        assert await package.applicable()

        # Check that the property works, installation status doesn't matter
        # (if running on the host machine system packages might or might not
        # be installed)
        assert await package.is_installed() in {True, False}

    @pytest.mark.trio
    @requires_driver
    async def test_has_name(
        self, make_driver: TestDriver  # pylint:disable=unused-argument
    ):
        package = parse_package(
            await self.constructor_args(), driver=self.driver, db=self.setup_db()
        )
        assert package.name != ""
