from typing import Optional

import trio
from pydantic import Field

from ..utils import allow_singular_none, url_version
from .installer import make_installer
from .manual_version import ManualVersion

INSTALLER_LOCK = trio.Lock()


class SystemPackage(ManualVersion):
    system: str
    url: Optional[str] = None
    auto_updates: bool = False
    services: list[str] = Field(default_factory=list, alias="service")
    services_val = allow_singular_none("services")

    async def installer(self):
        return await make_installer(self.driver)

    def derive_name(self) -> str:
        return self.system

    async def get_remote_version(self) -> str:
        if self.url:
            return await url_version(self.url)
        if self.auto_updates:
            return "latest"
        return await (await self.installer()).latest_version(self.system)

    async def local_version(self) -> Optional[str]:
        if self.url:
            return self.cached_version
        installer = await self.installer()
        version = await installer.installed_version(self.system)
        if self.auto_updates:
            return "latest" if version else None
        return version

    async def postinstall_linux(self):
        if self.services:
            await self.driver.run("sudo", "systemctl", "daemon-reload")
            for service in self.services:
                await self.driver.run("sudo", "systemctl", "enable", "--now", service)

    async def postinstall_macos(self):
        pass

    async def install(self) -> None:
        async with INSTALLER_LOCK:
            installer = await self.installer()
            if self.url:
                await installer.install(self.url)
                await self.cache_version()
            elif await self.local_version():
                await installer.upgrade(self.system)
            else:
                await installer.install(self.system)
        await (await self.driver.os()).switch(
            linux=self.postinstall_linux, macos=self.postinstall_macos
        )()

        await super().install()
