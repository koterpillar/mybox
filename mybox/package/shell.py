from pathlib import Path
from typing import AsyncIterable, Optional

from ..tracker import Tracker
from ..utils import async_cached
from .base import Package
from .system import SystemPackage

SHELLS_FILE = Path("/etc/shells")


class Shell(Package):
    shell: Path

    def derive_name(self) -> str:
        return "_shell"

    async def get_remote_version(self) -> str:
        return str(self.shell)

    async def get_local_version_linux(self) -> str:
        result = await self.driver.with_root(False).run_output(
            "getent", "passwd", await self.driver.username()
        )
        return result.split(":")[6]

    async def get_local_version_macos(self) -> str:
        result = await self.driver.with_root(False).run_output(
            "dscl", ".", "-read", f"/Users/{await self.driver.username()}", "UserShell"
        )
        return result.split(": ")[1]

    async def local_version(self) -> Optional[str]:
        return await (await self.driver.os()).switch(
            linux=self.get_local_version_linux,
            macos=self.get_local_version_macos,
        )()

    @async_cached
    async def all_shells(self) -> list[Path]:
        shells_file = await self.driver.read_file(SHELLS_FILE)
        return [Path(shell) for shell in shells_file.splitlines()]

    async def install(self, *, tracker: Tracker) -> None:
        if not await self.driver.is_file(self.shell):
            raise ValueError(f"{self.shell} does not exist.")

        if not await self.driver.is_executable(self.shell):
            raise ValueError(f"{self.shell} is not executable.")

        if self.shell not in await self.all_shells():
            await self.driver.with_root(True).run(
                "tee", "-a", SHELLS_FILE, input=str(self.shell).encode()
            )

        await self.driver.run("chsh", "-s", self.shell, show_output=True)

        await super().install(tracker=tracker)

    async def prerequisites(self) -> AsyncIterable[Package]:
        async for package in super().prerequisites():
            yield package  # pragma: no cover

        os = await self.driver.os()

        for system in os.switch_(
            linux=lambda linux: (
                ["util-linux-user"] if linux.distribution == "fedora" else []
            ),
            macos=lambda: [],
        ):
            yield SystemPackage(
                system=system,
                db=self.db,
                driver=self.driver_,
            )
