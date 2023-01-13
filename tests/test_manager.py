from pathlib import Path
from typing import Optional

import pytest

from mybox.driver import Driver, RunResult
from mybox.manager import Manager
from mybox.package.tracked import Tracked, Tracker
from mybox.state import DB, INSTALLED_FILES, InstalledFile
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


class DummyPackage(Tracked):
    def __init__(self, *, name_: str, files: list[str], **kwargs):
        super().__init__(**kwargs)
        self.name_ = name_
        self.files = files

    @property
    def name(self) -> str:
        return self.name_

    async def local_version(self) -> Optional[str]:
        return None

    async def get_remote_version(self) -> str:
        return "1"

    async def install_tracked(self, *, tracker: Tracker) -> None:
        for file in self.files:
            tracker.track(Path(file))
        await super().install_tracked(tracker=tracker)


class TestManager:
    @pytest.mark.trio
    async def test_removes_orphans(self):
        db = DB(":memory:")

        installed_files = INSTALLED_FILES(db)
        installed_files.append(InstalledFile(path="/foo", package="foo", root=False))
        installed_files.append(InstalledFile(path="/bar", package="bar", root=False))

        driver = DummyDriver()
        manager = Manager(db=db, driver=driver)
        packages = [
            DummyPackage(db=db, driver=driver, name_="foo", files=["/foo", "/baz"])
        ]
        await manager.install_packages(packages)

        assert set(installed_files.find()) == {
            InstalledFile(path="/foo", package="foo", root=False),
            InstalledFile(path="/baz", package="foo", root=False),
        }
        assert ["rm", "-r", "-f", "/bar"] in driver.commands
