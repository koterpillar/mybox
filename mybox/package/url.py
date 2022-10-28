from ..utils import url_version
from .archive import ArchivePackage


class URLPackage(ArchivePackage):
    def __init__(self, *, url: str, **kwargs) -> None:
        self.url = url
        super().__init__(**kwargs)

    async def archive_url(self) -> str:
        return self.url

    @property
    def name(self):
        parts = self.url.split("/")
        while True:
            if len(parts) == 0:
                raise ValueError(f"Cannot parse package name from {self.url}.")
            elif parts[0] in ("", "https:"):
                parts.pop(0)
            elif parts[0] == "github.com":
                return "/".join(parts[1:2])
            else:
                return parts[0]

    async def get_remote_version(self) -> str:
        return url_version(self.url)
