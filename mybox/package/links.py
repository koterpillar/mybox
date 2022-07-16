import hashlib
import os
from typing import Iterator

import yaml

from ..fs import files_in_recursively, link
from ..utils import *
from .manual_version import ManualVersion


class Links(ManualVersion):
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
        self.source = os.path.abspath(links)
        if not dest.startswith("/"):
            if self.root:
                dest = os.path.join(with_os(linux="/root", macos="/var/root"), dest)
            else:
                dest = os.path.join(home(), dest)
        self.dest = dest
        self.dot = dot
        self.shallow = shallow
        self.only = unsome_(only)
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return f"links-{self.source}-{self.dest}"

    def all_paths(self) -> Iterator[str]:
        if self.shallow:
            for entry in os.scandir(self.source):
                yield entry.path
        else:
            for path in files_in_recursively(self.source):
                if os.path.isfile(path):
                    yield path

    def paths(self) -> Iterator[str]:
        for path in self.all_paths():
            if self.only and os.path.basename(path) not in self.only:
                continue
            yield path

    def get_remote_version(self) -> str:
        m = hashlib.sha256()
        for path in self.paths():
            m.update(path.encode())
        return m.hexdigest()

    def install(self) -> None:
        # TODO: remove links that were created before but no longer exist
        for path in self.paths():
            target = os.path.relpath(path, self.source)
            if self.dot:
                target = "." + target
            target = os.path.join(self.dest, target)

            link(path, target, sudo=self.root)
        super().install()


def load_links() -> list[Links]:
    with open("links.yaml") as f:
        links = yaml.safe_load(f)
        return list(Links(**args) for args in links)
