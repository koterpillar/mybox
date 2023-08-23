import shlex
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional

from .driver import Driver
from .utils import async_cached


class Extractor(ABC):
    def __init__(self, *, driver: Driver):
        self.driver = driver

    @abstractmethod
    async def extract(
        self, *, archive: Path, target_directory: Path, strip: int = 0
    ) -> None:
        pass


class Tar(Extractor):
    def __init__(self, *, driver: Driver, extra: Optional[list[str]] = None):
        super().__init__(driver=driver)
        self.extra = extra or []

    @async_cached
    async def tar(self) -> str:
        return await self.driver.find_executable("gtar", "tar")

    async def extract(
        self, *, archive: Path, target_directory: Path, strip: int = 0
    ) -> None:
        await self.driver.run(
            await self.tar(),
            "--extract",
            "--strip",
            str(strip),
            "--directory",
            target_directory,
            *self.extra,
            "--file",
            archive,
        )


class Unzip(Extractor):
    async def extract(
        self, *, archive: Path, target_directory: Path, strip: int = 0
    ) -> None:
        if strip > 0:
            raise NotImplementedError("Strip is not supported for unzip.")
        await self.driver.run("unzip", "-o", "-qq", archive, "-d", target_directory)


class AppImage(Extractor):
    async def extract(
        self, *, archive: Path, target_directory: Path, strip: int = 0
    ) -> None:
        if strip > 0:
            raise NotImplementedError("Strip is not supported for AppImage.")

        async with self.driver.tempfile() as target:
            await self.driver.run("cp", archive, target)
            await self.driver.make_executable(target)

            cmd = " && ".join(
                [
                    shlex.join(["cd", str(target_directory)]),
                    shlex.join([str(target), "--appimage-extract"]),
                ]
            )
            await self.driver.run("sh", "-c", cmd)


def get_extractor(url: str, *, driver: Driver) -> Extractor:
    if url.endswith(".tar"):
        return Tar(driver=driver)
    elif url.endswith(".tar.gz") or url.endswith(".tgz"):
        return Tar(driver=driver, extra=["-z"])
    elif url.endswith(".tar.bz2"):
        return Tar(driver=driver, extra=["-j"])
    elif url.endswith(".tar.xz") or url.endswith(".txz"):
        return Tar(driver=driver, extra=["-J"])
    elif url.endswith(".zip"):
        return Unzip(driver=driver)
    elif url.endswith(".AppImage"):
        return AppImage(driver=driver)
    else:
        raise ValueError(f"Unknown archive format: {url}")
