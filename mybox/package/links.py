import hashlib
from pathlib import Path
from typing import Iterator

from ..utils import Some, unsome_
from .destination import Destination
from .manual_version import ManualVersion


class Links(ManualVersion, Destination):
    def __init__(
        self,
        links: str,
        dot: bool = False,
        shallow: bool = False,
        only: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.source = Path(links).absolute()
        self.dot = dot
        self.shallow = shallow
        self.only = unsome_(only)

    @property
    def name(self) -> str:
        return f"links-{self.source}-{self.destination}"

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
            target = self.destination.joinpath(target)

            self.driver.link(path, target)
        super().install()
