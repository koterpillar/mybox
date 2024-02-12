from pathlib import Path
from typing import Optional

import pytest
import yaml
from pydantic import Field

from mybox.driver import Driver, RunResult
from mybox.manager import Manager
from mybox.package.base import Package
from mybox.package.github import GitHubPackage
from mybox.package.manual_version import ManualVersion
from mybox.package.root import Root
from mybox.state import DB, INSTALLED_FILES, VERSIONS, InstalledFile
from mybox.tracker import Tracker
from mybox.utils import RunArg, allow_singular_none


class DummyDriver(Driver):
    commands: list[list[str]]

    def __init__(self, *, commands: Optional[list[list[str]]] = None, **kwargs) -> None:
        super().__init__(**kwargs)
        self.commands = commands if commands is not None else []

    def deconstruct(self) -> dict:
        return super().deconstruct() | {"commands": self.commands}

    def reset(self) -> None:
        self.commands[:] = []

    async def run_(
        self,
        *args: RunArg,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        show_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        output = ""

        args_ = [str(arg) for arg in args]

        if args_[0] == "uname":
            output = "Linux"
        elif args_[0] == "cat":
            if args_[1] == "/etc/os-release":
                output = "ID=ubuntu"

        if self.root:
            args_.insert(0, "sudo")

        self.commands.append(args_)

        return RunResult(ok=True, output=output)


class DummyPackage(Root, ManualVersion):
    files: list[str] = Field(default_factory=list)
    files_val = allow_singular_none("files")

    version: str = "1"
    error: Optional[Exception] = None

    async def get_remote_version(self) -> str:
        return self.version

    async def install(self, *, tracker: Tracker) -> None:
        if self.error is not None:
            raise self.error
        for file in self.files:
            tracker.track(Path(file), root=self.root)
        await super().install(tracker=tracker)


class TestManager:
    @classmethod
    def package_names(cls, packages: list[Package]) -> list[str]:
        return [package.name for package in packages]

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
        self.manager = Manager(db=self.db, driver=self.driver, component_path=tmp_path)

    def write_component(self, name: str, *packages: dict) -> None:
        with open(self.manager.component_path / f"{name}.yaml", "w") as out:
            yaml.dump(list(packages), out, indent=4)

    def make_package(
        self,
        name: str,
        *,
        files: Optional[list[str]] = None,
        root: bool = False,
        version: str = "1",
        error: Optional[Exception] = None,
    ) -> DummyPackage:
        return DummyPackage(
            db=self.db,
            driver=self.driver,
            name=name,
            files=files or [],
            root=root,
            version=version,
            error=error,
        )

    def installed_files(self) -> set[InstalledFile]:
        return set(INSTALLED_FILES(self.db).find())

    def versions(self) -> dict[str, str]:
        return {name: version.version for name, version in VERSIONS(self.db).find_ids()}

    @pytest.mark.trio
    async def test_tracks_packages_and_files(self):
        # Install packages onto an empty system; they install a shared file
        # and some files unique for each
        result1 = await self.manager.install_packages(
            [
                self.make_package("foo", files=["/shared", "/foo"]),
                self.make_package("bar", files=["/shared", "/bar"]),
                self.make_package("baz", files=["/baz"]),
                self.make_package("quux", files=["/quux"], root=True),
            ]
        )
        assert not result1.failed
        assert self.package_names(result1.installed) == ["foo", "bar", "baz", "quux"]

        expected_files = {
            InstalledFile(path="/shared", package="foo", root=False),
            InstalledFile(path="/shared", package="bar", root=False),
            InstalledFile(path="/foo", package="foo", root=False),
            InstalledFile(path="/bar", package="bar", root=False),
            InstalledFile(path="/baz", package="baz", root=False),
            InstalledFile(path="/quux", package="quux", root=True),
        }
        assert self.installed_files() == expected_files

        # Install an updated version for one package, keep one and remove others
        self.driver.reset()
        result2 = await self.manager.install_packages(
            [
                self.make_package(
                    "foo",
                    files=["/shared", "/foo", "/foo2"],
                    version="2",
                ),
                self.make_package("baz", files=["/baz"]),
            ]
        )
        assert not result2.failed
        assert self.package_names(result2.installed) == ["foo"]

        # Check package versions: foo should be updated, bar should be removed
        assert self.versions() == {"foo": "2", "baz": "1"}

        # Check installed files: everything from foo should be installed,
        # everything from bar removed, shared files left alone
        assert self.installed_files() == {
            InstalledFile(path="/shared", package="foo", root=False),
            InstalledFile(path="/shared", package="bar", root=False),
            InstalledFile(path="/foo", package="foo", root=False),
            InstalledFile(path="/foo2", package="foo", root=False),
            InstalledFile(path="/baz", package="baz", root=False),
        }
        assert ["rm", "-r", "-f", "/bar"] in self.driver.commands
        assert ["rm", "-r", "-f", "/shared"] not in self.driver.commands

        # File installed with root should be removed with root
        assert ["sudo", "rm", "-r", "-f", "/quux"] in self.driver.commands

    @pytest.mark.trio
    async def test_collects_errors(self):
        result = await self.manager.install_packages(
            [
                self.make_package(name="good1"),
                self.make_package(name="good2"),
                self.make_package(name="bad1", error=Exception("exc1")),
                self.make_package(name="bad2", error=Exception("exc2")),
            ]
        )

        assert self.package_names(result.installed) == ["good1", "good2"]
        assert {package.name: str(error) for package, error in result.failed} == {
            "bad1": "exc1",
            "bad2": "exc2",
        }

    def test_parses_packages(self):
        self.write_component("one", {"repo": "asdf/asdf", "binary": ["asdf"]})
        self.write_component("two", {"name": "foo"})

        packages = self.manager.load_components(frozenset(["one"]))

        assert len(packages) == 1
        package = packages[0]
        assert isinstance(package, GitHubPackage)
        assert package.repo == "asdf/asdf"
        assert package.binaries == ["asdf"]
