from typing import Optional

from pydantic import Field

from ..tracker import Tracker
from ..utils import RunArg, async_cached, repo_version
from .destination import Destination

DEFAULT_REMOTE = "origin"


class Clone(Destination):
    repo: str = Field(..., alias="clone")

    def derive_name(self) -> str:
        return self.repo

    async def directory_exists(self) -> bool:
        return await self.driver.is_dir(await self.destination())

    @async_cached
    async def git_args(self) -> list[RunArg]:
        return ["git", "-C", await self.destination()]

    async def run_git(self, *args: RunArg) -> None:
        await self.driver.run(*await self.git_args(), *args)

    async def run_git_output(self, *args: RunArg) -> str:
        return await self.driver.run_output(*await self.git_args(), *args)

    async def local_version(self) -> Optional[str]:
        if not await self.directory_exists():
            return None
        return await self.run_git_output("rev-parse", "HEAD")

    @property
    def remote(self):
        if self.repo.startswith("https://") or "@" in self.repo:
            return self.repo
        return f"https://github.com/{self.repo}.git"

    async def get_remote_version(self) -> str:
        return await repo_version(self.remote)

    async def branch_name(self, ref: str) -> str:
        return await self.run_git_output("rev-parse", "--abbrev-ref", ref)

    async def install(self, *, tracker: Tracker) -> None:
        destination = await self.destination()

        await self.driver.makedirs(destination.parent)

        if not await self.directory_exists():
            await self.driver.run("git", "clone", self.remote, destination)

        await self.run_git("remote", "set-url", DEFAULT_REMOTE, self.remote)

        default_remote_branch = await self.branch_name(f"{DEFAULT_REMOTE}/HEAD")
        default_branch = default_remote_branch.split("/")[1]
        await self.run_git("fetch", "--no-tags", DEFAULT_REMOTE, default_branch)

        current_branch = await self.branch_name("HEAD")
        if current_branch != default_branch:
            await self.run_git("switch", default_branch)
        await self.run_git("reset", "--hard", default_remote_branch)

        tracker.track(destination, root=self.root)

        await super().install(tracker=tracker)
