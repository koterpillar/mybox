import hashlib
from pathlib import Path
from typing import Iterator

from ..fs import home, link
from ..utils import *
from .manual_version import ManualVersion


class Links(ManualVersion):
    dest: Path

    def __init__(
        self,
        links: str,
        dest: str,
        dot: bool = False,
        root: bool = False,
        shallow: bool = False,
        only: Some[str] = None,
        **kwargs,
    ) -> None:
        self.root = root
        self.source = Path(links).absolute()
        if dest.startswith("/"):
            self.dest = Path(dest)
        else:
            if self.root:
                self.dest = Path("~root").expanduser() / dest
            else:
                self.dest = home() / dest
        self.dot = dot
        self.shallow = shallow
        self.only = unsome_(only)
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return f"links-{self.source}-{self.dest}"

    def all_paths(self) -> Iterator[Path]:
        if not self.source.is_dir():
            raise ValueError(f"Source is not a directory: {self.source}")

        if self.shallow:
            candidates = self.source.glob("*")
        else:
            candidates = self.source.rglob("*")

        for path in candidates:
            if path.is_file():
                yield path

    def paths(self) -> Iterator[Path]:
        for path in self.all_paths():
            if self.only and path.name not in self.only:
                continue
            yield path

    def get_remote_version(self) -> str:
        m = hashlib.sha256()
        for path in self.paths():
            m.update(str(path).encode())
        return m.hexdigest()

    def install(self) -> None:
        # TODO: remove links that were created before but no longer exist
        for path in self.paths():
            target = path.relative_to(self.source)
            if self.dot:
                first_part, *parts = target.parts
                target = Path("." + first_part, *parts)
            target = self.dest.joinpath(target)

            link(path, target, sudo=self.root)
        super().install()
