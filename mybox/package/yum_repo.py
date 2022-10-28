import configparser
import hashlib
from io import StringIO
from pathlib import Path

from ..utils import Optional, raise_
from .manual_version import ManualVersion


class YumRepo(ManualVersion):
    def __init__(
        self, yum_name: str, yum_url: str, gpg_key: Optional[str] = None, **kwargs
    ) -> None:
        super().__init__(**kwargs)
        self.repo_name = yum_name
        self.baseurl = yum_url
        self.gpg_key = gpg_key

    @property
    def name(self) -> str:
        return f"yum-{self.repo_name}"

    async def get_remote_version(self) -> str:
        m = hashlib.sha256()
        m.update(self.baseurl.encode())
        m.update((self.gpg_key or "").encode())
        return m.hexdigest()

    async def install(self) -> None:
        (await self.driver.os()).switch_(
            linux=lambda linux: lambda: None
            if linux.distribution == "fedora"
            else raise_(ValueError("YumRepo is only supported on Fedora")),
            macos=lambda: raise_(ValueError("YumRepo is only supported on Linux")),
        )()

        repo = configparser.ConfigParser()
        repo[self.repo_name] = {
            "name": self.repo_name,
            "baseurl": self.baseurl,
            "enabled": "1",
        }
        if self.gpg_key is not None:
            repo[self.repo_name]["gpgcheck"] = "1"
            repo[self.repo_name]["gpgkey"] = self.gpg_key

        contents = StringIO()
        repo.write(contents, space_around_delimiters=False)
        await self.driver.with_root(True).write_file(
            Path(f"/etc/yum.repos.d/{self.repo_name}.repo"), contents.getvalue()
        )

        if self.gpg_key:
            await self.driver.with_root(True).run("rpm", "--import", self.gpg_key)

        await super().install()
