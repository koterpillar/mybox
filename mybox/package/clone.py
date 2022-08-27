from pathlib import Path
from typing import Optional

from ..fs import home, makedirs
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
        return home() / self.destination

    @property
    def directory_exists(self) -> bool:
        return self.directory.is_dir()

    @property
    def local_version(self) -> Optional[str]:
        if not self.directory_exists:
            return None
        return run_output("git", "rev-parse", "HEAD", cwd=self.directory)

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    def get_remote_version(self) -> str:
        return run_output("git", "ls-remote", self.remote, "HEAD").split()[0]

    def install(self) -> None:
        makedirs(self.directory.parent)
        if not self.directory_exists:
            run("git", "clone", self.remote, str(self.directory))
        run("git", "remote", "set-url", "origin", self.remote, cwd=self.directory)
        run("git", "fetch", cwd=self.directory)
        default_branch = run_output(
            "git", "rev-parse", "--abbrev-ref", "origin/HEAD", cwd=self.directory
        ).split("/")[1]
        run("git", "switch", default_branch, cwd=self.directory)
        run("git", "reset", "--hard", f"origin/{default_branch}", cwd=self.directory)
