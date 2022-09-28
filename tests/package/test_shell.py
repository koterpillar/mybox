from typing import Iterable

from .base import PackageArgs, RootPackageTestBase


class TestShell(RootPackageTestBase):
    @property
    def constructor_args(self) -> PackageArgs:
        return {"shell": "/bin/sh"}

    skip_if_local = True

    @property
    def check_installed_command(self) -> Iterable[str]:
        return ["sh", "-c", f"cat /etc/passwd | grep {self.username}"]

    check_installed_output = "/bin/sh"


class TestRootShell(TestShell):
    root = True

    @property
    def constructor_args(self) -> PackageArgs:
        return super().constructor_args | {"root": True}
