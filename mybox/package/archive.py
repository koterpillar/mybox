import shlex
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Iterable

from ..configparser import DesktopEntry
from ..utils import async_cached
from .manual import ManualPackage
from .tracked import Tracker


class ArchivePackage(ManualPackage, ABC):
    raw: bool | str = False
    raw_executable: bool = False
    strip: int = 0

    @abstractmethod
    async def archive_url(self) -> str:
        pass

    @property
    def pathname(self) -> str:
        return self.name.replace("/", "--")

    @async_cached
    async def package_directory(self) -> Path:
        return (await self.local()) / "mybox" / self.pathname

    @async_cached
    async def tar(self) -> str:
        return await self.driver.find_executable("gtar", "tar")

    async def untar(self, source: Path, *extra: str) -> None:
        await self.driver.run(
            await self.tar(),
            "-x",
            "--strip",
            str(self.strip),
            "-C",
            await self.package_directory(),
            *extra,
            "-f",
            source,
        )

    async def unzip(self, source: Path) -> None:
        if self.strip > 0:
            raise NotImplementedError("Strip is not supported for unzip.")
        await self.driver.run(
            "unzip", "-o", "-qq", source, "-d", await self.package_directory()
        )

    async def appimage(self, source: Path) -> None:
        async with self.driver.tempfile() as target:
            await self.driver.run("cp", source, target)
            await self.driver.make_executable(target)

            cmd = " && ".join(
                [
                    shlex.join(["cd", str(await self.package_directory())]),
                    shlex.join([str(target), "--appimage-extract"]),
                ]
            )
            await self.driver.run("sh", "-c", cmd)

    async def extract(self, url: str, source: Path) -> None:
        if self.raw:
            if isinstance(self.raw, str):
                filename = self.raw
            else:
                filename = url.rsplit("/", 1)[-1]
            target = await self.package_directory() / filename
            await self.driver.run("cp", source, target)
            if self.raw_executable:
                await self.driver.make_executable(target)
        elif url.endswith(".tar"):
            await self.untar(source)
        elif url.endswith(".tar.gz") or url.endswith(".tgz"):
            await self.untar(source, "-z")
        elif url.endswith(".tar.bz2"):
            await self.untar(source, "-j")
        elif url.endswith(".tar.xz") or url.endswith(".txz"):
            await self.untar(source, "-J")
        elif url.endswith(".zip"):
            await self.unzip(source)
        elif url.endswith(".AppImage"):
            await self.appimage(source)
        else:
            raise ValueError(f"Unknown archive format: {url}")

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
            candidate_ok = await self.driver.is_file(candidate)
            if require_executable and candidate_ok:
                candidate_ok = await self.driver.is_executable(candidate)
            if candidate_ok:
                return candidate
        raise ValueError(
            f"Cannot find {target_desc} '{name}' in {await self.package_directory()}."
        )

    async def binary_path(self, binary: str) -> Path:
        return await self.find_in_package_directory(
            paths=[[], ["bin"]],
            name=binary,
            require_executable=True,
            target_desc="binary",
        )

    async def app_path(self, name: str) -> Path:
        return await self.find_in_package_directory(
            paths=[["share", "applications"]],
            name=f"{name}.desktop",
            target_desc="application",
        )

    async def icon_paths(self, name: str) -> Iterable[Path]:
        return await self.driver.find(
            await self.package_directory(), name=[f"{name}.svg", f"{name}.png"]
        )

    async def font_path(self, name: str) -> Path:
        candidate = await self.package_directory() / name
        if await self.driver.is_file(candidate):
            return candidate
        raise ValueError(
            f"Cannot find font '{name}' in {await self.package_directory()}."
        )

    async def install_appimage(self, tracker: Tracker) -> None:
        app_dir = await self.package_directory() / "squashfs-root"
        app_run = app_dir / "AppRun"
        if not await self.driver.is_executable(app_run):
            raise ValueError("AppImage does not have an executable named 'AppRun'.")

        binary_name = self.pathname
        binary_target = await self.local() / "bin" / binary_name
        await self.install_binary_wrapper(app_run, binary_target)
        tracker.track(binary_target)

        desktop_files = await self.driver.find(app_dir, name="*.desktop", maxdepth=1)
        if not desktop_files:
            raise ValueError("AppImage does not have a .desktop file.")
        if len(desktop_files) > 1:
            raise ValueError(f"AppImage has multiple .desktop files: {desktop_files}")
        desktop_file = desktop_files[0]

        desktop_entry = DesktopEntry.from_string(
            await self.driver.read_file(desktop_file)
        )
        desktop_entry.exec = " ".join(
            [binary_name, desktop_entry.exec.split(" ", 1)[1]]
        )

        target_desktop_file = await self.application_path() / desktop_file.name
        await self.driver.write_file(target_desktop_file, desktop_entry.to_string())
        tracker.track(target_desktop_file)
        if desktop_entry.icon:
            await self.install_icon(desktop_entry.icon, tracker)

    async def install_tracked(self, *, tracker: Tracker):
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
            tracker.track(package_directory.parent)
            tracker.track(package_directory)

            await self.extract(url, archive_path)

        if url.endswith(".AppImage"):
            await self.install_appimage(tracker)

        await super().install_tracked(tracker=tracker)
