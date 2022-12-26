import pytest

from .base import PackageArgs, PackageTestBase


class TestNodeSource(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {
            "yum_name": "nodesource",
            "yum_url": "https://rpm.nodesource.com/pub_19.x/fc/37/$basearch",
            "gpg_key": "https://rpm.nodesource.com/pub/el/NODESOURCE-GPG-SIGNING-KEY-EL",
        }

    async def check_installed_command(self):
        return ["yum", "info", "nodejs"]

    check_installed_output = "19."

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if not (await self.driver.os()).switch_(
            linux=lambda os: os.distribution == "fedora", macos=False
        ):
            pytest.skip("This test is only applicable on Fedora.")
