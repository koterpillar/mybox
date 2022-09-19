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
        if self.root:
            return run_ok("sudo", "test", "-d", str(self.destination))
        return self.destination.is_dir()

    @property
    def git_args(self) -> list[str]:
        result = ["git", "-C", str(self.destination)]
        if self.root:
            result.insert(0, "sudo")
        return result

    @property
    def local_version(self) -> Optional[str]:
        if not self.directory_exists:
            return None
        return run_output(*self.git_args, "rev-parse", "HEAD")

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    def get_remote_version(self) -> str:
        return run_output("git", "ls-remote", self.remote, "HEAD").split()[0]

    def install(self) -> None:
        makedirs(self.destination.parent, sudo=self.root)
        if not self.directory_exists:
            prefix = ["sudo"] if self.root else []
            run(*prefix, "git", "clone", self.remote, str(self.destination))
        run(*self.git_args, "remote", "set-url", "origin", self.remote)
        run(*self.git_args, "fetch")
        default_branch = run_output(
            *self.git_args, "rev-parse", "--abbrev-ref", "origin/HEAD"
        ).split("/")[1]
        run(*self.git_args, "switch", default_branch)
        run(*self.git_args, "reset", "--hard", f"origin/{default_branch}")
