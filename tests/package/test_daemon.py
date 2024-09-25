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
        uid = await self.driver.run_output("id", "-u")
        await self.driver.run(
            "launchctl", "bootout", f"gui/{uid}/com.koterpillar.mybox.sleep_3600"
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
