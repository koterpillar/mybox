import pytest

from .base import PackageArgs, PackageTestBase


class TestYumRepo(PackageTestBase):
    affects_system = True

    async def constructor_args(self) -> PackageArgs:
        return {
            "yum_name": "nodesource",
            "yum_url": "https://rpm.nodesource.com/pub_23.x/nodistro/nodejs/$basearch",
            "gpg_key": "https://rpm.nodesource.com/pub/el/NODESOURCE-GPG-SIGNING-KEY-EL",
        }

    async def check_installed_command(self):
        return ["yum", "info", "nodejs"]

    check_installed_output = "23."

    async def check_applicable(self) -> None:
        await super().check_applicable()
        (await self.driver.os()).switch_(
            linux=lambda os: (
                None
                if os.distribution == "fedora"
                else pytest.skip("YUM repo test is only applicable on Fedora")
            ),
            macos=lambda: pytest.skip("YUM repo test is only applicable on Linux"),
        )
