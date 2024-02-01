from abc import ABC, abstractmethod
from pathlib import Path
from typing import Iterable

from pydantic import Field

from ..configparser import DesktopEntry
from ..extractor import get_extractor
from ..tracker import Tracker
from ..utils import allow_singular_none, async_cached
from .manual import ManualPackage

ICON_EXTENSIONS = ["svg", "png"]

FONT_EXTENSIONS = ["ttf", "otf"]


class ArchivePackage(ManualPackage, ABC):
    raw: bool | str = False
    raw_executable: bool = False

    binary_paths: list[str] = Field(default_factory=list, alias="binary_path")
    binary_paths_val = allow_singular_none("binary_paths")

    @abstractmethod
    async def archive_url(self) -> str:
        pass

    @property
    def pathname(self) -> str:
        return self.name.replace("/", "--")

    @async_cached
    async def package_directory(self) -> Path:
        return (await self.local()) / "mybox" / self.pathname

    async def extract(self, url: str, source: Path) -> None:
        if self.raw:
            if isinstance(self.raw, str):
                filename = self.raw
            else:
                filename = url.rsplit("/", 1)[-1]
            target = await self.package_directory() / filename
            await self.driver.copy(source, target)
            if self.raw_executable:
                await self.driver.make_executable(target)
            return

        extractor = await get_extractor(url, driver=self.driver)
        await extractor.extract(
            archive=source, target_directory=await self.package_directory()
        )

    async def find_in_package_directory(
        self,
        paths: list[list[str]],
        name: str,
        *,
        require_executable: bool = False,
        target_desc: str,
    ) -> Path:
        for relative_path in paths:
            candidate = await self.package_directory() / Path(*relative_path) / name
            if not await self.driver.is_file(candidate):
                continue
            if require_executable and not await self.driver.is_executable(candidate):
                continue
            return candidate
        raise ValueError(
            f"Cannot find {target_desc} '{name}' in {await self.package_directory()}."
        )

    async def binary_path(self, binary: str) -> Path:
        return await self.find_in_package_directory(
            paths=[[], ["bin"], *([bp] for bp in self.binary_paths)],
            name=binary,
            require_executable=True,
            target_desc="binary",
        )

    async def freedesktop_app_path(self, name: str) -> Path:
        return await self.find_in_package_directory(
            paths=[["share", "applications"]],
            name=f"{name}.desktop",
            target_desc="application",
        )

    async def macos_app_path(self, name: str) -> Path:
        dir_name = f"{name}.app"
        candidates = await self.driver.find(
            await self.package_directory(), name=dir_name, file_type="d"
        )
        if candidates:
            return candidates[0]
        else:
            raise ValueError(
                f"Cannot find application '{name}' in {await self.package_directory()}."
            )

    @staticmethod
    def with_extensions(name: str, extensions: list[str]) -> list[str]:
        if any(name.endswith(f".{ext}") for ext in extensions):
            return [name]
        return [f"{name}.{ext}" for ext in extensions]

    async def icon_paths(self, name: str) -> Iterable[Path]:
        names = self.with_extensions(name, ICON_EXTENSIONS)
        return await self.driver.find(await self.package_directory(), name=names)

    async def font_path(self, name: str) -> Path:
        names = self.with_extensions(name, FONT_EXTENSIONS)
        candidates = await self.driver.find(await self.package_directory(), name=names)
        if candidates:
            return candidates[0]
        else:
            raise ValueError(
                f"Cannot find font '{name}' in {await self.package_directory()}."
            )

    async def install_appimage(self, tracker: Tracker) -> None:
        app_dir = await self.package_directory()
        app_run = app_dir / "AppRun"
        if not await self.driver.is_executable(app_run):
            raise ValueError("AppImage does not have an executable named 'AppRun'.")

        binary_name = self.pathname
        binary_target = await self.local() / "bin" / binary_name
        await self.install_binary_wrapper(app_run, binary_target)
        tracker.track(binary_target, root=self.root)

        desktop_files = await self.driver.find(app_dir, name="*.desktop", maxdepth=1)
        if not desktop_files:
            raise ValueError("AppImage does not have a .desktop file.")
        if len(desktop_files) > 1:
            raise ValueError(f"AppImage has multiple .desktop files: {desktop_files}")
        desktop_file = desktop_files[0]

        desktop_entry = DesktopEntry.from_string(
            await self.driver.read_file(desktop_file)
        )
        [_, *args] = desktop_entry.exec.split(" ")
        desktop_entry.exec = " ".join([binary_name, *args])

        target_desktop_file = await self.application_path() / desktop_file.name
        await self.driver.write_file(target_desktop_file, desktop_entry.to_string())
        tracker.track(target_desktop_file, root=self.root)
        if desktop_entry.icon:
            await self.install_icon(desktop_entry.icon, tracker)

    async def install(self, *, tracker: Tracker):
        url = await self.archive_url()
        async with self.driver.tempfile() as archive_path:
            await self.driver.run(
                "curl",
                "--silent",
                "--show-error",
                "--location",
                "--output",
                archive_path,
                url,
            )

            package_directory = await self.package_directory()
            await self.driver.makedirs(package_directory)
            tracker.track(package_directory.parent, root=self.root)
            tracker.track(package_directory, root=self.root)

            await self.extract(url, archive_path)

        if url.endswith(".AppImage"):
            await self.install_appimage(tracker)

        await super().install(tracker=tracker)
