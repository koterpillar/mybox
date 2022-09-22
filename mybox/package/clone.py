from typing import Optional

from ..fs import makedirs
from ..utils import run, run_ok, run_output
from .destination import Destination


class Clone(Destination):
    def __init__(self, clone: str, **kwargs) -> None:
        self.repo = clone
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return self.repo

    @property
    def directory_exists(self) -> bool:
        return run_ok("test", "-d", str(self.destination), sudo=self.root)

    @property
    def git_args(self) -> list[str]:
        return ["git", "-C", str(self.destination)]

    def run_git(self, *args: str) -> None:
        run(*self.git_args, *args, sudo=self.root)

    @property
    def local_version(self) -> Optional[str]:
        if not self.directory_exists:
            return None
        return run_output(*self.git_args, "rev-parse", "HEAD", sudo=self.root)

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    def get_remote_version(self) -> str:
        return run_output("git", "ls-remote", self.remote, "HEAD").split()[0]

    def install(self) -> None:
        makedirs(self.destination.parent, sudo=self.root)
        if not self.directory_exists:
            run("git", "clone", self.remote, str(self.destination), sudo=self.root)
        self.run_git("remote", "set-url", "origin", self.remote)
        self.run_git("fetch")
        default_branch = run_output(
            *self.git_args, "rev-parse", "--abbrev-ref", "origin/HEAD", sudo=self.root
        ).split("/")[1]
        self.run_git("switch", default_branch)
        self.run_git("reset", "--hard", f"origin/{default_branch}")
