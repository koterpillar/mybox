from typing import Optional

import trio

from ..utils import Some, unsome, url_version
from .installer import make_installer
from .manual_version import ManualVersion

INSTALLER_LOCK = trio.Lock()


class SystemPackage(ManualVersion):
    def __init__(
        self,
        *,
        name: str,
        url: Optional[str] = None,
        auto_updates: bool = False,
        service: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self._name = name
        self.url = url
        self.auto_updates = auto_updates
        self.services = unsome(service)

    async def installer(self):
        return await make_installer(self.driver)

    @property
    def name(self) -> str:
        return self._name

    async def get_remote_version(self) -> str:
        if self.url:
            return url_version(self.url)
        if self.auto_updates:
            return "latest"
        return await (await self.installer()).latest_version(self.name)

    async def local_version(self) -> Optional[str]:
        if self.url:
            return self.cached_version
        installer = await self.installer()
        version = await installer.installed_version(self.name)
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

    async def install(self):
        async with INSTALLER_LOCK:
            installer = await self.installer()
            if self.url:
                await installer.install(self.url)
                await self.cache_version()
            elif await self.local_version():
                await installer.upgrade(self.name)
            else:
                await installer.install(self.name)
        await (await self.driver.os()).switch(
            linux=self.postinstall_linux, macos=self.postinstall_macos
        )()
