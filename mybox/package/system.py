from typing import Optional

import trio
from pydantic import Field

from ..compute import Value
from ..tracker import Tracker
from ..utils import allow_singular_none, async_cached, url_version
from .installer import make_installer
from .manual_version import ManualVersion

INSTALLER_LOCK = trio.Lock()


class SystemPackage(ManualVersion):
    system: str
    url_: Optional[Value] = Field(default=None, alias="url")
    auto_updates: bool = False
    services: list[str] = Field(default_factory=list, alias="service")
    services_val = allow_singular_none("services")

    async def installer(self):
        return await make_installer(self.driver)

    def derive_name(self) -> str:
        return self.system

    @async_cached
    async def url(self) -> Optional[str]:
        if self.url_:
            return await self.url_.compute()
        return None

    async def get_remote_version(self) -> str:
        if url := await self.url():
            return await url_version(url)
        if self.auto_updates:
            return "latest"
        return await (await self.installer()).latest_version(self.system)

    async def local_version(self) -> Optional[str]:
        if await self.url():
            return self.cached_version
        installer = await self.installer()
        version = await installer.installed_version(self.system)
        if self.auto_updates:
            return "latest" if version else None
        return version

    async def postinstall_linux(self):
        if self.services:
            await self.driver.with_root(True).run("systemctl", "daemon-reload")
            for service in self.services:
                await self.driver.with_root(True).run("systemctl", "enable", service)

    async def postinstall_macos(self):
        pass

    async def install(self, *, tracker: Tracker) -> None:
        async with INSTALLER_LOCK:
            installer = await self.installer()
            if url := await self.url():
                await installer.install(url)
                await self.cache_version()
            elif await self.local_version():
                await installer.upgrade(self.system)
            else:
                await installer.install(self.system)
        await (await self.driver.os()).switch(
            linux=self.postinstall_linux, macos=self.postinstall_macos
        )()

        await super().install(tracker=tracker)
