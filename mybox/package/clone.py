from typing import Optional

from ..utils import RunArg, async_cached, run_output
from .destination import Destination
from .tracked import Tracked, Tracker


class Clone(Destination, Tracked):
    def __init__(self, clone: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.repo = clone

    @property
    def name(self) -> str:
        return self.repo

    async def directory_exists(self) -> bool:
        return await self.driver.run_ok("test", "-d", await self.destination())

    @async_cached
    async def git_args(self) -> list[RunArg]:
        return ["git", "-C", await self.destination()]

    async def run_git(self, *args: RunArg) -> None:
        await self.driver.run(*await self.git_args(), *args)

    async def local_version(self) -> Optional[str]:
        if not await self.directory_exists():
            return None
        return await self.driver.run_output(*await self.git_args(), "rev-parse", "HEAD")

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    async def get_remote_version(self) -> str:
        return (await run_output("git", "ls-remote", self.remote, "HEAD")).split()[0]

    async def install_tracked(self, *, tracker: Tracker) -> None:
        destination = await self.destination()

        await self.driver.makedirs(destination.parent)

        if not await self.directory_exists():
            await self.driver.run("git", "clone", self.remote, destination)

        await self.run_git("remote", "set-url", "origin", self.remote)
        await self.run_git("fetch")
        default_branch = (
            await self.driver.run_output(
                *await self.git_args(), "rev-parse", "--abbrev-ref", "origin/HEAD"
            )
        ).split("/")[1]
        await self.run_git("switch", default_branch)
        await self.run_git("reset", "--hard", f"origin/{default_branch}")

        tracker.track(destination)

        await super().install_tracked(tracker=tracker)
