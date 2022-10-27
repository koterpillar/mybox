from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Optional, Union

from ..utils import async_cached
from .manual import ManualPackage


class ArchivePackage(ManualPackage, metaclass=ABCMeta):
    def __init__(
        self,
        *,
        raw: Union[bool, str] = False,
        raw_executable: bool = False,
        strip: int = 0,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.raw = raw
        self.raw_executable = raw_executable
        self.strip = strip

    @abstractmethod
    async def archive_url(self) -> str:
        pass

    async def package_directory(self) -> Path:
        result = (await self.local()) / "mybox" / self.name.replace("/", "--")
        await self.driver.makedirs(result)
        return result

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
        else:
            raise ValueError(f"Unknown archive format: {url}")

    async def binary_path(self, binary: str) -> Path:
        paths: list[list[str]] = [[], ["bin"]]
        for relative_path in paths:
            candidate = await self.package_directory() / Path(*relative_path) / binary
            if await self.driver.is_executable(candidate):
                return candidate
        raise ValueError(f"Cannot find {binary} in {await self.package_directory()}.")

    async def app_path(self, name: str) -> Path:
        candidate = (
            await self.package_directory()
            / "share"
            / "applications"
            / f"{name}.desktop"
        )
        if await self.driver.is_file(candidate):
            return candidate
        raise ValueError(
            f"Cannot find application '{name}' in {await self.package_directory()}."
        )

    async def icon_directory(self) -> Optional[Path]:
        candidate = await self.package_directory() / "share" / "icons"
        if await self.driver.is_dir(candidate):
            return candidate
        return None

    async def font_path(self, name: str) -> Path:
        candidate = await self.package_directory() / name
        if await self.driver.is_file(candidate):
            return candidate
        raise ValueError(
            f"Cannot find font '{name}' in {await self.package_directory()}."
        )

    async def install(self):
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
            await self.extract(url, archive_path)
        await super().install()
