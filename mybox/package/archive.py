import shlex
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Iterable, Union

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

    @property
    def pathname(self) -> str:
        return self.name.replace("/", "--")

    async def package_directory(self) -> Path:
        result = (await self.local()) / "mybox" / self.pathname
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
            if require_executable:
                candidate_ok = await self.driver.is_executable(candidate)
            else:
                candidate_ok = await self.driver.is_file(candidate)
            if candidate_ok:
                return candidate
        raise ValueError(
            f"Cannot find {target_desc} '{name}' in {await self.package_directory()}."
        )

    async def binary_path(self, binary: str) -> Path:
        appimage_candidate = await self.package_directory() / "squashfs-root" / "AppRun"
        if await self.driver.is_executable(appimage_candidate):
            return appimage_candidate
        return await self.find_in_package_directory(
            paths=[[], ["bin"]],
            name=binary,
            require_executable=True,
            target_desc="binary",
        )

    async def app_path(self, name: str) -> Path:
        return await self.find_in_package_directory(
            paths=[["share", "applications"], ["squashfs-root"]],
            name=f"{name}.desktop",
            target_desc="application",
        )

    async def icon_paths(self, name: str) -> Iterable[Path]:
        return [
            Path(path)
            for path in (
                await self.driver.run_output(
                    "find",
                    await self.package_directory(),
                    "-name",
                    f"{name}.svg",
                    "-o",
                    "-name",
                    f"{name}.png",
                )
            ).splitlines()
        ]

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
