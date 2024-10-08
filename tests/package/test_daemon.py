import pytest

from mybox.driver import LocalDriver
from mybox.package import Daemon
from mybox.state import DB

from ..base import DOCKER
from .base import PackageArgs, PackageTestBase


class TestDaemon(PackageTestBase):
    async def cleanup(self):
        await (await self.driver.os()).switch(
            linux=self.cleanup_linux, macos=self.cleanup_macos
        )()

    async def cleanup_linux(self) -> None:
        await self.driver.rm(
            await self.driver.local()
            / "share"
            / "systemd"
            / "user"
            / "com.koterpillar.mybox.sleep_3600.service"
        )
        await self.driver.run("systemctl", "--user", "daemon-reload")

    async def cleanup_macos(self) -> None:
        daemons = await self.driver.run_output("launchctl", "list")
        if "Mybox" in daemons:
            await self.driver.run(
                "launchctl", "unload", "com.koterpillar.mybox.sleep_3600"
            )
        await self.driver.rm(
            await self.driver.home()
            / "Library"
            / "LaunchAgents"
            / "com.koterpillar.mybox.sleep_3600.plist"
        )

    async def constructor_args(self) -> PackageArgs:
        return {"daemon": ["sleep", "3600"]}

    async def check_installed_command(self):
        return (await self.driver.os()).switch(
            linux=["systemctl", "--user", "list-units"],
            macos=["launchctl", "list"],
        )

    check_installed_output = "Mybox: sleep 3600"

    async def check_applicable(self) -> None:
        await super().check_applicable()
        if DOCKER:
            pytest.skip("Daemon relies on systemd, which is not available in Docker.")


def test_daemon_name_description():
    package = Daemon(
        daemon=["echo", "multiple words", "привет мир", "/etc/passwd"],
        driver=LocalDriver(),
        db=DB.temporary(),
    )

    assert (
        package.daemon_name
        == "com.koterpillar.mybox.echo_multiple_words_привет_мир__etc_passwd"
    )
    assert (
        package.daemon_description
        == "Mybox: echo 'multiple words' 'привет мир' /etc/passwd"
    )


def test_explicit_daemon_name():
    package = Daemon(
        name="My daemon",
        daemon=["echo", "hello"],
        driver=LocalDriver(),
        db=DB.temporary(),
    )

    assert package.daemon_description == "Mybox: My daemon"
