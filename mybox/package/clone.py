import os
from contextlib import contextmanager
from pathlib import Path
from typing import Iterator, Optional

from ..fs import HOME
from ..utils import *
from .base import Package


class Clone(Package):
    def __init__(self, clone: str, destination: str, **kwargs) -> None:
        self.repo = clone
        self.destination = destination
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return self.repo

    @property
    def directory(self) -> Path:
        return HOME / self.destination

    @property
    def directory_exists(self) -> bool:
        return self.directory.is_dir()

    @contextmanager
    def in_directory(self) -> Iterator[None]:
        olddir = Path.cwd()
        try:
            os.chdir(self.directory)
            yield
        finally:
            os.chdir(olddir)

    @property
    def local_version(self) -> Optional[str]:
        if not self.directory_exists:
            return None
        with self.in_directory():
            return run_output("git", "rev-parse", "HEAD")

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    def get_remote_version(self) -> str:
        return run_output("git", "ls-remote", self.remote, "HEAD").split()[0]

    def install(self) -> None:
        if not self.directory_exists:
            run("git", "clone", self.remote, str(self.directory))
        with self.in_directory():
            run("git", "remote", "set-url", "origin", self.remote)
            run("git", "fetch")
            default_branch = run_output(
                "git", "rev-parse", "--abbrev-ref", "origin/HEAD"
            ).split("/")[1]
            run("git", "switch", default_branch)
            run("git", "reset", "--hard", f"origin/{default_branch}")
