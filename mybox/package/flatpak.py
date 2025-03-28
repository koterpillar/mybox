from functools import cached_property
from typing import Optional

import trio

from ..tracker import Tracker
from .installer.flatpak import Flatpak
from .manual_version import ManualVersion

FLATPAK_LOCK = trio.Lock()


class FlatpakPackage(ManualVersion):
    flatpak: str

    def derive_name(self) -> str:
        return self.flatpak

    @cached_property
    def installer(self) -> Flatpak:
        return Flatpak(self.driver)

    async def get_remote_version(self) -> str:
        return await self.installer.latest_version(self.flatpak)

    async def local_version(self) -> Optional[str]:
        return await self.installer.installed_version(self.flatpak)

    async def install(self, *, tracker: Tracker) -> None:
        async with FLATPAK_LOCK:
            if await self.local_version():
                # Cannot install an old version of a package in tests
                await self.installer.upgrade(self.flatpak)  # pragma: no cover
            else:
                await self.installer.install(self.flatpak)

        await super().install(tracker=tracker)
