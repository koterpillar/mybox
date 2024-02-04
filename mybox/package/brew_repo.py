from pydantic import Field

from ..tracker import Tracker
from ..utils import Optional, async_cached, raise_
from .installer import Brew
from .manual_version import ManualVersion


class BrewRepo(ManualVersion):
    repo_name: str = Field(alias="brew_tap")

    def derive_name(self) -> str:
        return f"brew-{self.repo_name}"

    async def get_remote_version(self) -> str:
        return "installed"

    async def local_version(self) -> Optional[str]:
        brew = await self.brew()
        return "installed" if self.repo_name in await brew.tapped() else None

    @async_cached
    async def brew(self) -> Brew:
        return Brew(self.driver)

    async def install(self, *, tracker: Tracker) -> None:
        (await self.driver.os()).switch_(
            linux=lambda _: raise_(ValueError("BrewRepo is only supported on macOS")),
            macos=lambda: None,
        )

        brew = await self.brew()
        await brew.tap(self.repo_name)

        await super().install(tracker=tracker)
