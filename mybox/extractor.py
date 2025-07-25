import shlex
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Optional

from .driver import Driver
from .utils import async_cached


class Extractor(ABC):
    def __init__(self, *, driver: Driver):
        self.driver = driver

    async def extract(self, *, archive: Path, target_directory: Path) -> None:
        async with self.driver.tempfile(kind="directory") as tmpdir:
            await self.extract_exact(archive=archive, target_directory=tmpdir)

            source_dir = tmpdir
            stripped = 0

            while True:
                contents = await self.driver.find(source_dir, maxdepth=1)
                if len(contents) != 1:
                    # Multiple files/directories on this level
                    break

                element = contents[0]
                if not await self.driver.is_dir(element):
                    # The only item is a file
                    break

                source_dir = source_dir / element
                stripped += 1
                if stripped >= 10:
                    raise ValueError(
                        f"Too many nested directories after extracting, got {source_dir}."
                    )

            for element in contents:
                await self.driver.copy(element, target_directory / element.name)

    @abstractmethod
    async def extract_exact(self, *, archive: Path, target_directory: Path) -> None:
        pass


class Tar(Extractor):
    def __init__(self, *, driver: Driver, extra: Optional[list[str]] = None):
        super().__init__(driver=driver)
        self.extra = extra or []

    @async_cached
    async def tar(self) -> str:
        return await self.driver.find_executable("gtar", "tar")

    async def extract_exact(self, *, archive: Path, target_directory: Path) -> None:
        await self.driver.run(
            await self.tar(),
            "--extract",
            "--directory",
            target_directory,
            *self.extra,
            "--file",
            archive,
        )


class Unzip(Extractor):
    async def extract_exact(self, *, archive: Path, target_directory: Path) -> None:
        await self.driver.run("unzip", "-o", "-qq", archive, "-d", target_directory)


def _guess_extractor(url: str, *, driver: Driver) -> Extractor:
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
    else:
        raise ValueError(f"Unknown archive format: {url}")


async def get_redirect_url(url: str, *, driver: Driver) -> str:
    return await driver.run_output(
        "curl",
        "--write-out",
        "%{url_effective}",
        "--head",
        "--no-include",
        "--fail",
        "--silent",
        "--show-error",
        "--location",
        url,
    )


async def get_extractor(url: str, *, driver: Driver) -> Extractor:
    try:
        return _guess_extractor(url, driver=driver)
    except ValueError:
        url = await get_redirect_url(url, driver=driver)
        return _guess_extractor(url, driver=driver)


class RawExtractor(ABC):
    def __init__(self, *, driver: Driver):
        self.driver = driver

    @abstractmethod
    async def extract(self, *, archive: Path, target: Path) -> None:
        pass


class ShellRedirectRawExtractor(RawExtractor):
    def __init__(self, *, driver: Driver, command: str):
        super().__init__(driver=driver)
        self.command = command

    async def extract(self, *, archive: Path, target: Path) -> None:
        await self.driver.run(
            "sh",
            "-c",
            f"{self.command} < {shlex.quote(str(archive))} > {shlex.quote(str(target))}",
        )


class Move(RawExtractor):
    async def extract(self, *, archive: Path, target: Path) -> None:
        await self.driver.copy(archive, target)


def _guess_single_extractor(url: str, *, driver: Driver) -> RawExtractor:
    if url.endswith(".gz"):
        return ShellRedirectRawExtractor(driver=driver, command="gunzip")
    elif url.endswith(".xz"):
        return ShellRedirectRawExtractor(driver=driver, command="xzcat")
    elif url.endswith(".bz2"):
        return ShellRedirectRawExtractor(driver=driver, command="bunzip2")
    else:
        raise ValueError(f"Unknown archive format: {url}")


async def get_single_extractor(url: str, *, driver: Driver) -> RawExtractor:
    try:
        return _guess_single_extractor(url, driver=driver)
    except ValueError:
        url = await get_redirect_url(url, driver=driver)
        try:
            return _guess_single_extractor(url, driver=driver)
        except ValueError:
            return Move(driver=driver)
