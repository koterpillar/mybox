import configparser
import hashlib
import tempfile

from ..utils import *
from .manual_version import ManualVersion


class YumRepo(ManualVersion):
    def __init__(
        self, yum_name: str, yum_url: str, gpg_key: Optional[str] = None, **kwargs
    ) -> None:
        self.repo_name = yum_name
        self.baseurl = yum_url
        self.gpg_key = gpg_key
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return f"yum-{self.repo_name}"

    def get_remote_version(self) -> str:
        m = hashlib.sha256()
        m.update(self.baseurl.encode())
        m.update((self.gpg_key or "").encode())
        return m.hexdigest()

    def install(self) -> None:
        if CURRENT_OS != "linux":
            raise Exception("YumRepo is only supported on Linux")
        if CURRENT_DISTRIBUTION != "fedora":
            raise Exception("YumRepo is only supported on Fedora")

        repo = configparser.ConfigParser()
        repo[self.repo_name] = {
            "name": self.repo_name,
            "baseurl": self.baseurl,
            "enabled": "1",
        }
        if self.gpg_key is not None:
            repo[self.repo_name]["gpgcheck"] = "1"
            repo[self.repo_name]["gpgkey"] = self.gpg_key

        with tempfile.NamedTemporaryFile(mode="w") as repo_file:
            repo.write(repo_file, space_around_delimiters=False)
            repo_file.flush()
            run(
                "sudo",
                "install",
                "--mode",
                "644",
                repo_file.name,
                f"/etc/yum.repos.d/{self.repo_name}.repo",
            )

        if self.gpg_key:
            run("sudo", "rpm", "--import", self.gpg_key)

        super().install()
