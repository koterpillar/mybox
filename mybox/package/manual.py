import re
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Iterable

from pydantic import Field

from ..configparser import DesktopEntry
from ..tracker import Tracker
from ..utils import allow_singular_none
from .manual_version import ManualVersion

RESOLUTION_RE = re.compile(r"(\d+)x(\d+)")


class ManualPackage(ManualVersion, ABC):
    binaries: list[str] = Field(default_factory=list, alias="binary")
    binaries_val = allow_singular_none("binaries")

    binary_wrapper: bool = False

    apps: list[str] = Field(default_factory=list, alias="app")
    apps_val = allow_singular_none("apps")

    fonts: list[str] = Field(default_factory=list, alias="font")
    fonts_val = allow_singular_none("fonts")

    @abstractmethod
    async def binary_path(self, binary: str) -> Path:
        pass

    async def install_binary(self, name: str, tracker: Tracker) -> None:
        binary = await self.binary_path(name)
        target = await self.driver.local() / "bin" / name
        if self.binary_wrapper:
            await self.install_binary_wrapper(binary, target)
        else:
            await self.driver.link(binary, target)
        tracker.track(target, root=self.root)

    async def install_binary_wrapper(self, binary: Path, target: Path) -> None:
        await self.driver.write_file(target, f'#!/bin/sh\nexec "{binary}" "$@"')
        await self.driver.make_executable(target)

    @abstractmethod
    async def freedesktop_app_path(self, name: str) -> Path:
        pass

    @abstractmethod
    async def icon_paths(self, name: str) -> Iterable[Path]:
        pass

    async def icon_target_path(self, path: Path) -> Path:
        resolution: str
        if path.suffix == ".svg":
            resolution = "scalable"
        elif path.suffix == ".png":
            for part in path.parts:
                if RESOLUTION_RE.match(part):
                    resolution = part
                    break
            else:
                # Give a low resolution to prefer ones given explicitly
                resolution = "16x16"
        else:
            raise ValueError(f"Unknown icon type: '{path}'")

        return (
            (await self.driver.local())
            / "share"
            / "icons"
            / "hicolor"
            / resolution
            / "apps"
            / path.name
        )

    async def install_app(self, name: str, tracker: Tracker) -> None:
        await (await self.driver.os()).switch(
            linux=self.install_app_linux, macos=self.install_app_macos
        )(name, tracker)

    async def application_path(self) -> Path:
        return await self.driver.local() / "share" / "applications"

    async def install_desktop_file(self, path: Path, tracker: Tracker) -> None:
        target = await self.application_path() / path.name
        await self.driver.link(path, target)
        tracker.track(target, root=self.root)
        desktop_entry = DesktopEntry.from_string(await self.driver.read_file(target))
        if desktop_entry.icon:
            await self.install_icon(desktop_entry.icon, tracker)

    async def install_icon(self, icon: str, tracker: Tracker) -> None:
        for icon_path in await self.icon_paths(icon):
            target = await self.icon_target_path(icon_path)
            await self.driver.link(icon_path, target)
            tracker.track(target, root=self.root)

    async def install_app_linux(self, name: str, tracker: Tracker) -> None:
        path = await self.freedesktop_app_path(name)
        await self.install_desktop_file(path, tracker)

    async def install_app_macos(self, name: str, tracker: Tracker) -> None:
        # Installing macOS apps manually is not implemented.
        pass  # pragma: no cover

    @abstractmethod
    async def font_path(self, name: str) -> Path:
        pass

    async def install_font(self, name: str, tracker: Tracker) -> None:
        font_dir = (await self.driver.os()).switch(
            linux=await self.driver.local() / "share" / "fonts",
            macos=await self.driver.home() / "Library" / "Fonts",
        )
        source = await self.font_path(name)
        target = font_dir / source.name
        await self.driver.copy(source, target)
        tracker.track(target, root=self.root)
        if await self.driver.executable_exists("fc-cache"):
            await self.driver.run("fc-cache", "-f", font_dir)

    @abstractmethod
    async def install(self, *, tracker: Tracker) -> None:
        for binary in self.binaries:
            await self.install_binary(binary, tracker)
        for app in self.apps:
            await self.install_app(app, tracker)
        for font in self.fonts:
            await self.install_font(font, tracker)

        await super().install(tracker=tracker)
