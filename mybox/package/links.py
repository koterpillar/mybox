import hashlib
from pathlib import Path
from typing import Iterator, Optional

from pydantic import Field, field_validator

from ..tracker import Tracker
from ..utils import allow_singular
from .destination import Destination
from .manual_version import ManualVersion


class Links(ManualVersion, Destination):
    source: Path = Field(..., alias="links")

    @field_validator("source")
    @classmethod
    def source_absolute(cls, value):
        return Path(value).absolute()

    dot: bool = False
    shallow: bool = False
    only: Optional[list[str]] = None
    only_val = allow_singular("only")

    def derive_name(self) -> str:
        return f"links-{self.source}-{self.destination_}-{self.dot}-{self.root}"

    def all_paths(self) -> Iterator[Path]:
        if not self.source.is_dir():
            raise ValueError(f"Source is not a directory: {self.source}")

        if self.shallow:
            return (path for path in self.source.glob("*"))
        else:
            return (path for path in self.source.rglob("*") if path.is_file())

    def paths(self) -> Iterator[Path]:
        for path in self.all_paths():
            if self.only and path.name not in self.only:
                continue
            yield path

    async def get_remote_version(self) -> str:
        m = hashlib.sha256()
        for path in self.paths():
            m.update(str(path).encode())
        return m.hexdigest()

    async def install(self, *, tracker: Tracker) -> None:
        destination = await self.destination()

        for path in self.paths():
            target = path.relative_to(self.source)
            if self.dot:
                first_part, *parts = target.parts
                target = Path("." + first_part, *parts)
            target = destination.joinpath(target)

            await self.driver.link(path, target)
            tracker.track(target, root=self.root)

        await super().install(tracker=tracker)
