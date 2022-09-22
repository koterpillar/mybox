import tempfile
from abc import ABCMeta, abstractmethod
from functools import cached_property
from pathlib import Path
from typing import Optional, Union

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
    def archive_url(self) -> str:
        pass

    def package_directory(self) -> Path:
        result = self.fs.local() / f"{self.name.replace('/', '--')}.app"
        self.fs.makedirs(result)
        return result

    @cached_property
    def tar(self) -> str:
        return self.fs.find_executable("gtar", "tar")

    def untar(self, source: Path, *extra: str) -> None:
        self.fs.run(
            self.tar,
            "-x",
            "--strip",
            str(self.strip),
            "-C",
            str(self.package_directory()),
            *extra,
            "-f",
            str(source),
        )

    def unzip(self, source: Path) -> None:
        if self.strip > 0:
            raise NotImplementedError("Strip is not supported for unzip.")
        self.fs.run("unzip", "-qq", str(source), "-d", str(self.package_directory()))

    def extract(self, url: str, source: Path) -> None:
        if self.raw:
            if isinstance(self.raw, str):
                filename = self.raw
            else:
                filename = url.rsplit("/", 1)[-1]
            target = self.package_directory() / filename
            self.fs.run("cp", str(source), str(target))
            if self.raw_executable:
                self.fs.make_executable(target)
        elif url.endswith(".tar"):
            self.untar(source)
        elif url.endswith(".tar.gz") or url.endswith(".tgz"):
            self.untar(source, "-z")
        elif url.endswith(".tar.bz2"):
            self.untar(source, "-j")
        elif url.endswith(".txz"):
            self.untar(source, "-J")
        elif url.endswith(".zip"):
            self.unzip(source)
        else:
            raise ValueError(f"Unknown archive format: {url}")

    def binary_path(self, binary: str) -> Path:
        paths: list[list[str]] = [[], ["bin"]]
        for relative_path in paths:
            candidate = self.package_directory() / Path(*relative_path) / binary
            if candidate.is_file() and self.fs.is_executable(candidate):
                return candidate
        raise ValueError(f"Cannot find {binary} in {self.package_directory()}.")

    def app_path(self, name: str) -> Path:
        candidate = (
            self.package_directory() / "share" / "applications" / f"{name}.desktop"
        )
        if candidate.is_file():
            return candidate
        raise ValueError(
            f"Cannot find application '{name}' in {self.package_directory()}."
        )

    def icon_directory(self) -> Optional[Path]:
        candidate = self.package_directory() / "share" / "icons"
        if candidate.is_dir():
            return candidate
        return None

    def font_path(self, name: str) -> Path:
        candidate = self.package_directory() / name
        if candidate.is_file():
            return candidate
        raise ValueError(f"Cannot find font '{name}' in {self.package_directory()}.")

    def install(self):
        url = self.archive_url()
        with tempfile.NamedTemporaryFile() as archive_file:
            self.fs.run("curl", "-sSL", url, stdout=archive_file)
            self.extract(url, Path(archive_file.name))
        super().install()
