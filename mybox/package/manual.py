import configparser
import re
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Iterable, Optional

from ..utils import Some, async_cached, unsome
from .manual_version import ManualVersion
from .root import Root


class ManualPackage(Root, ManualVersion, metaclass=ABCMeta):
    binaries: list[str]
    apps: list[str]
    fonts: list[str]

    def __init__(
        self,
        *,
        binary: Some[str] = None,
        binary_wrapper: bool = False,
        app: Some[str] = None,
        font: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.binaries = unsome(binary)
        self.binary_wrapper = binary_wrapper
        self.apps = unsome(app)
        self.fonts = unsome(font)

    @async_cached
    async def local(self) -> Path:
        if self.root:
            return Path("/usr/local")
        else:
            return await self.driver.local()

    @abstractmethod
    async def binary_path(self, binary: str) -> Path:
        pass

    async def install_binary(self, name: str) -> None:
        await self.driver.link(
            await self.binary_path(name),
            await self.local() / "bin" / name,
            method="binary_wrapper" if self.binary_wrapper else None,
        )

    @abstractmethod
    async def app_path(self, name: str) -> Path:
        pass

    @abstractmethod
    async def icon_paths(self, name: str) -> Iterable[Path]:
        pass

    RESOLUTION_RE = re.compile(r"(\d+)x(\d+)")

    async def icon_target_path(self, path: Path) -> Path:
        resolution: str
        if path.suffix == ".svg":
            resolution = "scalable"
        elif path.suffix == ".png":
            for part in path.parts:
                if self.RESOLUTION_RE.match(part):
                    resolution = part
                    break
            else:
                # Give a low resolution to prefer ones given explicitly
                resolution = "16x16"
        else:
            raise ValueError(f"Unknown icon type: '{path}'")

        return (
            (await self.local())
            / "share"
            / "icons"
            / "hicolor"
            / resolution
            / "apps"
            / path.name
        )

    async def icon_name(self, app_path: Path) -> Optional[str]:
        config = await self.driver.read_file(app_path)
        app = configparser.ConfigParser()
        app.read_string(config)
        return app["Desktop Entry"].get("Icon")

    async def install_app(self, name: str) -> None:
        await (await self.driver.os()).switch(
            linux=self.install_app_linux, macos=self.install_app_macos
        )(name)

    async def install_app_linux(self, name: str) -> None:
        path = await self.app_path(name)
        target = await self.local() / "share" / "applications" / f"{name}.desktop"
        await self.driver.link(path, target)

        icon = await self.icon_name(path)
        if icon:
            for icon_path in await self.icon_paths(icon):
                target = await self.icon_target_path(icon_path)
                await self.driver.link(icon_path, target)

    async def install_app_macos(self, name: str) -> None:
        # FIXME: copy to /Applications and/or ~/Applications; ensure names,
        # etc. are correct
        pass

    @abstractmethod
    async def font_path(self, name: str) -> Path:
        pass

    async def install_font(self, name: str) -> None:
        font_dir = (await self.driver.os()).switch(
            linux=await self.local() / "share" / "fonts",
            macos=await self.driver.home() / "Library" / "Fonts",
        )
        await self.driver.makedirs(font_dir)
        source = await self.font_path(name)
        target = font_dir / name
        await self.driver.link(source, target)
        if await self.driver.executable_exists("fc-cache"):
            await self.driver.run("fc-cache", "-f", font_dir)

    async def install(self) -> None:
        for binary in self.binaries:
            await self.install_binary(binary)
        for app in self.apps:
            await self.install_app(app)
        for font in self.fonts:
            await self.install_font(font)
        await super().install()
