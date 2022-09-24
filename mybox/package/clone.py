from typing import Optional

from ..utils import run_output
from .destination import Destination


class Clone(Destination):
    def __init__(self, clone: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.repo = clone

    @property
    def name(self) -> str:
        return self.repo

    @property
    def directory_exists(self) -> bool:
        return self.driver.run_ok("test", "-d", str(self.destination))

    @property
    def git_args(self) -> list[str]:
        return ["git", "-C", str(self.destination)]

    def run_git(self, *args: str) -> None:
        self.driver.run(*self.git_args, *args)

    @property
    def local_version(self) -> Optional[str]:
        if not self.directory_exists:
            return None
        return self.driver.run_output(*self.git_args, "rev-parse", "HEAD")

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    def get_remote_version(self) -> str:
        return run_output("git", "ls-remote", self.remote, "HEAD").split()[0]

    def install(self) -> None:
        self.driver.makedirs(self.destination.parent)
        if not self.directory_exists:
            self.driver.run("git", "clone", self.remote, str(self.destination))
        self.run_git("remote", "set-url", "origin", self.remote)
        self.run_git("fetch")
        default_branch = self.driver.run_output(
            *self.git_args, "rev-parse", "--abbrev-ref", "origin/HEAD"
        ).split("/")[1]
        self.run_git("switch", default_branch)
        self.run_git("reset", "--hard", f"origin/{default_branch}")
