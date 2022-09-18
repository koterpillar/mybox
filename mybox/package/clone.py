from typing import Optional

from ..fs import makedirs
from ..utils import run, run_output
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
        return self.destination.is_dir()

    @property
    def local_version(self) -> Optional[str]:
        if not self.directory_exists:
            return None
        return run_output("git", "rev-parse", "HEAD", cwd=self.destination)

    @property
    def remote(self):
        return f"https://github.com/{self.repo}.git"

    def get_remote_version(self) -> str:
        return run_output("git", "ls-remote", self.remote, "HEAD").split()[0]

    def install(self) -> None:
        makedirs(self.destination.parent)
        if not self.directory_exists:
            run("git", "clone", self.remote, str(self.destination))
        run("git", "remote", "set-url", "origin", self.remote, cwd=self.destination)
        run("git", "fetch", cwd=self.destination)
        default_branch = run_output(
            "git", "rev-parse", "--abbrev-ref", "origin/HEAD", cwd=self.destination
        ).split("/")[1]
        run("git", "switch", default_branch, cwd=self.destination)
        run("git", "reset", "--hard", f"origin/{default_branch}", cwd=self.destination)
