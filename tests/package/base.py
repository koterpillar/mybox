import subprocess
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Any, Iterable, Optional

import pytest

from mybox.driver import Driver, LocalDriver
from mybox.package import parse_package
from mybox.state import DB


class OverrideHomeDriver(LocalDriver):
    def __init__(self, *, root: bool = False, override_home: Path) -> None:
        super().__init__(root=root)
        self.override_home = override_home

    def deconstruct(self) -> dict:
        return super().deconstruct() | {"override_home": self.override_home}

    def home(self) -> Path:
        if self.root:
            return super().home()
        return self.override_home


class PackageTestBase(metaclass=ABCMeta):
    driver: Driver

    @property
    @abstractmethod
    def constructor_args(self) -> dict[str, Any]:
        pass

    @property
    @abstractmethod
    def check_installed_command(self) -> Iterable[str]:
        pass

    check_installed_output: Optional[str] = None

    @pytest.fixture(autouse=True)
    def ensure_local_bin_environment(
        self, monkeypatch: pytest.MonkeyPatch, tmp_path: Path
    ) -> None:
        local_bin = tmp_path / ".local" / "bin"
        monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")
        self.driver = OverrideHomeDriver(override_home=tmp_path)

    def check_installed(self):
        result = subprocess.run(
            self.check_installed_command, check=True, stdout=subprocess.PIPE
        )
        if self.check_installed_output is not None:
            output = result.stdout.decode()
            assert self.check_installed_output in output

    def test_installs(self):
        db = DB(":memory:")
        package = parse_package(self.constructor_args, db=db, driver=self.driver)
        assert package.applicable
        package.install()
        self.check_installed()
