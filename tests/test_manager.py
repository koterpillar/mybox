import tempfile
from pathlib import Path
from typing import Optional

import pytest
import yaml

from mybox.driver import Driver, RunResult
from mybox.manager import Manager
from mybox.package import GitHubPackage
from mybox.package.manual_version import ManualVersion
from mybox.package.tracked import Tracked, Tracker
from mybox.state import DB, INSTALLED_FILES, VERSIONS, InstalledFile, Version
from mybox.utils import RunArg


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
    def __init__(self, *, version: str = "1", name_: str, files: list[str], **kwargs):
        super().__init__(**kwargs)
        self.version = version
        self.name_ = name_
        self.files = files

    @property
    def name(self) -> str:
        return self.name_

    async def get_remote_version(self) -> str:
        return self.version

    async def install_tracked(self, *, tracker: Tracker) -> None:
        for file in self.files:
            tracker.track(Path(file))
        await super().install_tracked(tracker=tracker)


class TestManager:
    @pytest.mark.trio
    async def test_removes_orphans(self):
        db = DB(":memory:")

        driver = DummyDriver()
        manager = Manager(db=db, driver=driver, component_path=Path("/dev/null"))

        # Install packages onto an empty system; they install a shared file
        # and some files unique for each
        await manager.install_packages(
            [
                DummyPackage(db=db, driver=driver, name_="foo", files=["/", "/foo"]),
                DummyPackage(db=db, driver=driver, name_="bar", files=["/", "/bar"]),
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
                    name_="foo",
                    files=["/", "/foo", "/baz"],
                    version="2",
                )
            ]
        )

        # Check package versions: foo should be updated, bar should be removed
        versions = VERSIONS(db)
        assert set(versions.find_ids()) == {
            ("foo", Version(version="2")),
        }

        # Check installed files
        installed_files = INSTALLED_FILES(db)
        assert set(installed_files.find()) == {
            InstalledFile(path="/", package="foo", root=False),
            InstalledFile(path="/foo", package="foo", root=False),
            InstalledFile(path="/baz", package="foo", root=False),
        }
        assert ["rm", "-r", "-f", "/bar"] in driver.commands
        assert ["rm", "-r", "-f", "/"] not in driver.commands

    def test_parses_packages(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            def write_component(name: str, *packages: dict) -> None:
                with open(tmppath / f"{name}.yaml", "w") as out:
                    yaml.dump(list(packages), out, indent=4)

            write_component("one", {"repo": "asdf/asdf", "binary": ["asdf"]})
            write_component("two", {"name": "foo"})

            db = DB(":memory:")
            driver = DummyDriver()
            manager = Manager(db=db, driver=driver, component_path=tmppath)

            packages = manager.load_components({"one"})

            assert len(packages) == 1
            package = packages[0]
            assert isinstance(package, GitHubPackage)
            assert package.repo == "asdf/asdf"
            assert package.binaries == ["asdf"]
