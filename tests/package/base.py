import sqlite3
import subprocess
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Any, Iterable, Optional

import pytest

from mybox.package import parse_package


class PackageTestBase(metaclass=ABCMeta):
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
        self, monkeypatch: pytest.MonkeyPatch, override_home: Path
    ) -> None:
        monkeypatch.setenv("MYBOX_HOME", str(override_home))
        local_bin = override_home / ".local" / "bin"
        monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")

    def check_installed(self):
        result = subprocess.run(
            self.check_installed_command, check=True, stdout=subprocess.PIPE
        )
        if self.check_installed_output is not None:
            output = result.stdout.decode()
            assert self.check_installed_output in output

    def test_installs(self):
        db = sqlite3.Connection(":memory:")
        package = parse_package(db, self.constructor_args)
        assert package.applicable
        package.install()
        self.check_installed()
