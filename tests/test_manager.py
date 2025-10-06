from collections.abc import AsyncIterable
from pathlib import Path
from typing import Any, Optional

import pytest
import yaml
from pydantic import Field, ValidationError

from mybox.manager import InstallResult, Manager
from mybox.package.base import Package
from mybox.package.manual_version import ManualVersion
from mybox.package.system import SystemPackage
from mybox.state import DB, INSTALLED_FILES, VERSIONS, InstalledFile
from mybox.tracker import Tracker
from mybox.utils import allow_singular_none

from .base import DummyDriver


class DummyPackage(ManualVersion):
    files: list[str] = Field(default_factory=list)
    files_val = allow_singular_none("files")

    version: str = "1"
    error: Optional[Exception] = None
    version_error: Optional[Exception] = None

    prerequisite_packages: Optional[list[str]] = None

    async def get_remote_version(self) -> str:
        if self.version_error is not None:
            raise self.version_error
        return self.version

    async def install(self, *, tracker: Tracker) -> None:
        if self.error is not None:
            raise self.error
        for file in self.files:
            tracker.track(Path(file), root=self.root)
        await super().install(tracker=tracker)

    async def prerequisites(self) -> AsyncIterable[Package]:
        for name in self.prerequisite_packages or []:
            yield DummyPackage(
                db=self.db,
                driver=self.driver_,
                name=name,
                files=[],
                version="1",
            )


class DummyManager(Manager):
    """Dummy manager that reports all packages installed without doing anything"""

    async def install_packages(self, packages: list[Package]) -> InstallResult:
        return InstallResult(installed=packages, failed=[])


class TestManager:
    @classmethod
    def package_names(cls, result: InstallResult) -> list[str]:
        return [package.name for package in result.installed]

    db: DB
    driver: DummyDriver
    manager: Manager

    @pytest.fixture(autouse=True)
    def make_db(self) -> None:
        self.db = DB.temporary()

    @pytest.fixture(autouse=True)
    def make_driver(self) -> None:
        self.driver = DummyDriver()

    @pytest.fixture(autouse=True)
    def make_manager(self, tmp_path: Path) -> None:
        self.manager = Manager(db=self.db, driver=self.driver, data_path=tmp_path)

    def write_component_raw(self, name: str, contents: Any) -> None:
        (self.manager.data_path / "packages").mkdir(exist_ok=True)
        with open(self.manager.data_path / "packages" / f"{name}.yaml", "w") as out:
            yaml.dump(contents, out, indent=4)

    def write_component(self, name: str, *packages: dict) -> None:
        self.write_component_raw(name, list(packages))

    def make_package(
        self,
        name: str,
        *,
        files: Optional[list[str]] = None,
        root: bool = False,
        version: str = "1",
        error: Optional[Exception] = None,
        version_error: Optional[Exception] = None,
        prerequisites: Optional[list[str]] = None,
    ) -> DummyPackage:
        return DummyPackage(
            db=self.db,
            driver=self.driver,
            name=name,
            files=files or [],
            root=root,
            version=version,
            error=error,
            version_error=version_error,
            prerequisite_packages=prerequisites,
        )

    def installed_files(self) -> set[InstalledFile]:
        return set(INSTALLED_FILES(self.db).find())

    def versions(self) -> dict[str, str]:
        return {name: version.version for name, version in VERSIONS(self.db).find_ids()}

    async def install(self, *packages: DummyPackage) -> InstallResult:
        self.driver.reset()
        return await self.manager.install_packages(list(packages))

    async def install_assert(self, *packages: DummyPackage) -> InstallResult:
        result = await self.install(*packages)
        result.raise_for_failures()
        return result

    @pytest.mark.trio
    async def test_result_versions_for_new_packages(self):
        result = await self.install_assert(self.make_package("foo"))

        assert self.package_names(result) == ["foo"]
        assert self.versions() == {"foo": "1"}

    @pytest.mark.trio
    async def test_result_versions_for_added_packages(self):
        await self.install_assert(self.make_package("foo"))
        result = await self.install_assert(
            self.make_package("foo"), self.make_package("bar")
        )

        assert self.package_names(result) == ["bar"]
        assert self.versions() == {"foo": "1", "bar": "1"}

    @pytest.mark.trio
    async def test_result_versions_for_removed_packages(self):
        await self.install_assert(self.make_package("foo"))
        result = await self.install_assert()

        assert self.package_names(result) == []
        assert self.versions() == {}

    @pytest.mark.trio
    async def test_result_versions_for_updated_packages(self):
        await self.install_assert(self.make_package("foo"))
        result = await self.install_assert(self.make_package("foo", version="2"))

        assert self.package_names(result) == ["foo"]
        assert self.versions() == {"foo": "2"}

    @pytest.mark.trio
    async def test_result_versions_for_failed_packages(self):
        await self.install_assert(self.make_package("foo"))
        result = await self.install(
            self.make_package("foo", version="2", error=Exception("foo error")),
            self.make_package("bar", error=Exception("bar error")),
        )

        assert self.package_names(result) == []
        assert {package.name: str(error) for package, error in result.failed} == {
            "foo": "foo error",
            "bar": "bar error",
        }
        assert self.versions() == {"foo": "1"}

    @pytest.mark.trio
    async def test_result_versions_for_version_check_failed_packages(self):
        await self.install_assert(self.make_package("foo"))
        result = await self.install(
            self.make_package("foo", version="2", version_error=Exception("foo error")),
            self.make_package("bar", error=Exception("bar error")),
        )

        assert self.package_names(result) == []
        assert {package.name: str(error) for package, error in result.failed} == {
            "foo": "foo error",
            "bar": "bar error",
        }
        assert self.versions() == {"foo": "1"}

    @pytest.mark.trio
    async def test_result_versions_for_partial_failure(self):
        await self.install_assert(self.make_package("foo"))
        result = await self.install(
            self.make_package("foo", version="2"),
            self.make_package("bar", error=Exception("bar error")),
        )

        assert self.package_names(result) == ["foo"]
        assert {package.name: str(error) for package, error in result.failed} == {
            "bar": "bar error",
        }
        assert self.versions() == {"foo": "2"}

    @pytest.mark.trio
    async def test_result_versions_has_prerequisites(self):
        result = await self.install_assert(
            self.make_package("foo", prerequisites=["bar"])
        )
        assert self.package_names(result) == ["bar", "foo"]

    @pytest.mark.trio
    async def test_tracks_files_installed(self):
        await self.install_assert(self.make_package("foo", files=["/foo", "/bar"]))
        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=False),
            InstalledFile(path="/bar", package="foo", root=False),
        }

    @pytest.mark.trio
    async def test_tracks_files_removed(self):
        await self.install_assert(self.make_package("foo", files=["/foo", "/bar"]))
        await self.install_assert(self.make_package("foo", files=["/foo"], version="2"))

        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=False)
        }
        assert ["rm", "-r", "-f", "/bar"] in self.driver.commands

    @pytest.mark.trio
    async def test_tracks_files_installed_with_root(self):
        await self.install_assert(self.make_package("foo", files=["/foo"], root=True))

        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=True)
        }

        await self.install_assert()

        assert self.installed_files() == set()
        assert ["sudo", "rm", "-r", "-f", "/foo"] in self.driver.commands

    @pytest.mark.trio
    async def test_tracks_shared_files(self):
        await self.install_assert(
            self.make_package("foo", files=["/shared"]),
            self.make_package("bar", files=["/shared"]),
        )

        assert self.installed_files() == {
            InstalledFile(path="/shared", package="foo", root=False),
            InstalledFile(path="/shared", package="bar", root=False),
        }

        await self.install_assert(
            self.make_package("foo", files=["/shared"]),
        )

        assert self.installed_files() == {
            InstalledFile(path="/shared", package="foo", root=False),
        }
        assert ["rm", "-r", "-f", "/shared"] not in self.driver.commands

        await self.install_assert()
        assert self.installed_files() == set()
        assert ["rm", "-r", "-f", "/shared"] in self.driver.commands

    @pytest.mark.trio
    async def test_keeps_files_for_errored_packages(self):
        await self.install_assert(self.make_package("foo", files=["/foo"]))

        await self.install(
            self.make_package(
                "foo", files=["/foo"], error=Exception("foo error"), version="2"
            ),
        )

        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=False)
        }
        assert ["rm", "-r", "-f", "/foo"] not in self.driver.commands

        await self.install_assert(self.make_package("foo", files=["/foo"], version="3"))

        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=False)
        }
        assert ["rm", "-r", "-f", "/foo"] not in self.driver.commands

    @pytest.mark.trio
    async def test_keeps_files_for_version_errored_packages(self):
        await self.install_assert(self.make_package("foo", files=["/foo"]))

        await self.install(
            self.make_package(
                "foo", files=["/foo"], version_error=Exception("foo error"), version="2"
            ),
        )

        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=False)
        }
        assert ["rm", "-r", "-f", "/foo"] not in self.driver.commands

        await self.install_assert(self.make_package("foo", files=["/foo"], version="3"))

        assert self.installed_files() == {
            InstalledFile(path="/foo", package="foo", root=False)
        }
        assert ["rm", "-r", "-f", "/foo"] not in self.driver.commands

    def test_parses_packages(self):
        self.write_component(
            "one",
            {"system": "alpha"},
        )
        self.write_component("two", {"name": "foo"})

        packages = self.manager.load_components(frozenset(["one"]))

        assert len(packages) == 1
        (package1,) = packages
        assert isinstance(package1, SystemPackage)
        assert package1.name == "alpha"

    def test_parse_package_error_not_a_dict(self):
        self.write_component_raw("one", ["not", "a", "dict"])

        with pytest.raises(ValidationError):
            self.manager.load_components(frozenset(["one"]))

    def test_parse_package_error_invalid_dict(self):
        self.write_component("one", ["not", "a", "package"])

        with pytest.raises(ValidationError):
            self.manager.load_components(frozenset(["one"]))

    def test_parse_package_error_invalid_keys(self):
        self.write_component("one", {"invalid": "package"})

        with pytest.raises(ValidationError):
            self.manager.load_components(frozenset(["one"]))

    @pytest.mark.trio
    async def test_load_config_and_components(self):
        dummy_manager = DummyManager(
            db=self.db, driver=self.driver, data_path=self.manager.data_path
        )

        with open(self.manager.data_path / "mybox.yaml", "w") as out:
            yaml.dump(
                [
                    {"host": "*", "component": "one"},
                    {"host": "never", "component": "two"},
                ],
                out,
            )

        self.write_component("one", {"system": "one"})
        self.write_component("two", {"system": "two"})

        result = await dummy_manager.install()
        assert len(result.installed) == 1
        assert result.installed[0].name == "one"
