from urllib.parse import urlparse

from ..utils import url_version
from .archive import ArchivePackage


class URLPackage(ArchivePackage):
    url: str

    async def archive_url(self) -> str:
        return self.url

    def derive_name(self) -> str:
        url = urlparse(self.url)

        name = url.hostname or ""

        path = url.path.rsplit("/", 1)[-1]
        if path:
            name += "/" + path.split(".")[0]
        return name

    async def get_remote_version(self) -> str:
        return url_version(self.url)
