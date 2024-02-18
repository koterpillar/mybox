import hashlib
from pathlib import Path

from pydantic import Field

from ..configparser import ConfigParser
from ..tracker import Tracker
from ..utils import Optional, raise_
from .manual_version import ManualVersion


class YumRepo(ManualVersion):
    repo_name: str = Field(alias="yum_name")
    baseurl: str = Field(alias="yum_url")
    gpg_key: Optional[str] = None

    def derive_name(self) -> str:
        return f"yum-{self.repo_name}"

    async def get_remote_version(self) -> str:
        m = hashlib.sha256()
        m.update(self.baseurl.encode())
        m.update((self.gpg_key or "").encode())
        return m.hexdigest()

    async def install(self, *, tracker: Tracker) -> None:
        (await self.driver.os()).switch_(
            linux=lambda linux: (
                None
                if linux.distribution == "fedora"
                else raise_(ValueError("YumRepo is only supported on Fedora"))
            ),
            macos=lambda: raise_(ValueError("YumRepo is only supported on Linux")),
        )

        repo = ConfigParser()
        repo[self.repo_name] = {
            "name": self.repo_name,
            "baseurl": self.baseurl,
            "enabled": "1",
        }
        if self.gpg_key is not None:
            repo[self.repo_name]["gpgcheck"] = "1"
            repo[self.repo_name]["gpgkey"] = self.gpg_key

        driver = self.driver.with_root(True)
        path = Path(f"/etc/yum.repos.d/{self.repo_name}.repo")
        await driver.write_file(path, repo.to_string())
        await driver.run("chmod", "a+r", path)
        tracker.track(path, root=True)

        if self.gpg_key:
            await self.driver.with_root(True).run("rpm", "--import", self.gpg_key)

        await super().install(tracker=tracker)
