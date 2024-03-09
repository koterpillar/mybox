from abc import ABC, abstractmethod
from pathlib import Path
from typing import Iterable

from pydantic import Field

from ..extractor import get_extractor
from ..tracker import Tracker
from ..utils import allow_singular_none, async_cached, with_extensions
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

    async def icon_paths(self, name: str) -> Iterable[Path]:
        names = with_extensions(name, ICON_EXTENSIONS)
        return await self.driver.find(await self.package_directory(), name=names)

    async def font_path(self, name: str) -> Path:
        names = with_extensions(name, FONT_EXTENSIONS)
        candidates = await self.driver.find(await self.package_directory(), name=names)
        if candidates:
            return candidates[0]
        else:
            raise ValueError(
                f"Cannot find font '{name}' in {await self.package_directory()}."
            )

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

        await super().install(tracker=tracker)
