import tempfile
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
from mybox.package.tracked import Tracked, Tracker
from mybox.state import DB, INSTALLED_FILES, VERSIONS, InstalledFile, Version
from mybox.utils import RunArg, allow_singular_none


class DummyDriver(Driver):
    commands: list[list[str]]

    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.commands = []

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

        self.commands.append(args_)

        return RunResult(ok=True, output=output)


class DummyPackage(ManualVersion, Tracked):
    files: list[str] = Field(default_factory=list)
    files_val = allow_singular_none("files")

    version: str = "1"
    error: Optional[Exception] = None

    async def get_remote_version(self) -> str:
        return self.version

    async def install_tracked(self, *, tracker: Tracker) -> None:
        if self.error is not None:
            raise self.error
        for file in self.files:
            tracker.track(Path(file))
        await super().install_tracked(tracker=tracker)


class TestManager:
    @classmethod
    def package_names(cls, packages: list[Package]) -> list[str]:
        return [package.name for package in packages]

    @pytest.mark.trio
    async def test_returns_installed(self):
        db = DB.temporary()

        driver = DummyDriver()
        manager = Manager(db=db, driver=driver, component_path=Path("/dev/null"))

        await manager.install_packages(
            [
                DummyPackage(db=db, driver=driver, name="foo"),
                DummyPackage(db=db, driver=driver, name="bar"),
            ]
        )

        driver = DummyDriver()
        manager = Manager(db=db, driver=driver, component_path=Path("/dev/null"))

        result = await manager.install_packages(
            [
                DummyPackage(db=db, driver=driver, name="foo"),
                DummyPackage(db=db, driver=driver, name="bar", version="2"),
            ]
        )

        assert not result.failed
        assert self.package_names(result.installed) == ["bar"]

    @pytest.mark.trio
    async def test_removes_orphans(self):
        db = DB.temporary()

        driver = DummyDriver()
        manager = Manager(db=db, driver=driver, component_path=Path("/dev/null"))

        # Install packages onto an empty system; they install a shared file
        # and some files unique for each
        await manager.install_packages(
            [
                DummyPackage(
                    db=db, driver=driver, name="foo", files=["/shared", "/foo"]
                ),
                DummyPackage(
                    db=db, driver=driver, name="bar", files=["/shared", "/bar"]
                ),
            ]
        )

        # Only install one of the packages; the other should be removed
        driver = DummyDriver()
        manager = Manager(db=db, driver=driver, component_path=Path("/dev/null"))

        await manager.install_packages(
            [
                DummyPackage(
                    db=db,
                    driver=driver,
                    name="foo",
                    files=["/shared", "/foo", "/baz"],
                    version="2",
                )
            ]
        )

        # Check package versions: foo should be updated, bar should be removed
        versions = VERSIONS(db)
        assert set(versions.find_ids()) == {
            ("foo", Version(version="2")),
        }

        # Check installed files: everything from foo should be installed,
        # everything from bar removed, shared files left alone
        installed_files = INSTALLED_FILES(db)
        assert set(installed_files.find()) == {
            InstalledFile(path="/shared", package="foo", root=False),
            InstalledFile(path="/foo", package="foo", root=False),
            InstalledFile(path="/baz", package="foo", root=False),
        }
        assert ["rm", "-r", "-f", "/bar"] in driver.commands
        assert ["rm", "-r", "-f", "/shared"] not in driver.commands

    @pytest.mark.trio
    async def test_collects_errors(self):
        db = DB.temporary()

        driver = DummyDriver()
        manager = Manager(db=db, driver=driver, component_path=Path("/dev/null"))

        result = await manager.install_packages(
            [
                DummyPackage(db=db, driver=driver, name="good1"),
                DummyPackage(db=db, driver=driver, name="good2"),
                DummyPackage(
                    db=db, driver=driver, name="bad1", error=Exception("exc1")
                ),
                DummyPackage(
                    db=db, driver=driver, name="bad2", error=Exception("exc2")
                ),
            ]
        )

        assert self.package_names(result.installed) == ["good1", "good2"]
        assert [f"{package.name}: {error}" for package, error in result.failed] == [
            "bad1: exc1",
            "bad2: exc2",
        ]

    def test_parses_packages(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            def write_component(name: str, *packages: dict) -> None:
                with open(tmppath / f"{name}.yaml", "w") as out:
                    yaml.dump(list(packages), out, indent=4)

            write_component("one", {"repo": "asdf/asdf", "binary": ["asdf"]})
            write_component("two", {"name": "foo"})

            db = DB.temporary()
            driver = DummyDriver()
            manager = Manager(db=db, driver=driver, component_path=tmppath)

            packages = manager.load_components({"one"})

            assert len(packages) == 1
            package = packages[0]
            assert isinstance(package, GitHubPackage)
            assert package.repo == "asdf/asdf"
            assert package.binaries == ["asdf"]
