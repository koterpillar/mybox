from pathlib import Path
from typing import Any, Optional

import pytest
import yaml
from pydantic import ValidationError

from mybox.manager import InstallResult, Manager
from mybox.package.base import Package
from mybox.package.manual_version import ManualVersion
from mybox.package.system import SystemPackage
from mybox.state import DB, VERSIONS

from .base import DummyDriver


class DummyPackage(ManualVersion):
    version: str = "1"
    error: Optional[Exception] = None
    version_error: Optional[Exception] = None

    async def local_version(self) -> Optional[str]:
        return self.cached_version

    async def get_remote_version(self) -> str:
        if self.version_error is not None:
            raise self.version_error
        return self.version

    async def install(self) -> None:
        if self.error is not None:
            raise self.error
        await self.cache_version()
        await super().install()


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
        version: str = "1",
        error: Optional[Exception] = None,
        version_error: Optional[Exception] = None,
    ) -> DummyPackage:
        return DummyPackage(
            db=self.db,
            driver=self.driver,
            name=name,
            version=version,
            error=error,
            version_error=version_error,
        )

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
                    {"host": "*", "component": ["one"]},
                    {"host": "never", "component": ["two"]},
                ],
                out,
            )

        self.write_component("one", {"system": "one"})
        self.write_component("two", {"system": "two"})

        result = await dummy_manager.install()
        assert len(result.installed) == 1
        assert result.installed[0].name == "one"
