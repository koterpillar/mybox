from pathlib import Path
from typing import Optional, Union

from ..utils import async_cached, run_output
from .destination import Destination


class Clone(Destination):
    def __init__(self, clone: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.repo = clone

    @property
    def name(self) -> str:
        return self.repo

    async def directory_exists(self) -> bool:
        return await self.driver.run_ok("test", "-d", await self.destination())

    @async_cached
    async def git_args(self) -> list[Union[str, Path]]:
        return ["git", "-C", await self.destination()]

    async def run_git(self, *args: Union[str, Path]) -> None:
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

    async def install(self) -> None:
        await self.driver.makedirs((await self.destination()).parent)
        if not await self.directory_exists():
            await self.driver.run("git", "clone", self.remote, await self.destination())
        await self.run_git("remote", "set-url", "origin", self.remote)
        await self.run_git("fetch")
        default_branch = (
            await self.driver.run_output(
                *await self.git_args(), "rev-parse", "--abbrev-ref", "origin/HEAD"
            )
        ).split("/")[1]
        await self.run_git("switch", default_branch)
        await self.run_git("reset", "--hard", f"origin/{default_branch}")
