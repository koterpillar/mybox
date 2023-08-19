from urllib.parse import urlparse

from ..utils import url_version
from .archive import ArchivePackage


class URLPackage(ArchivePackage):
    url: str

    async def archive_url(self) -> str:
        return self.url

    @property
    def name(self):
        url = urlparse(self.url)

        name = url.hostname

        path = url.path.rsplit("/", 1)[-1]
        if path:
            name += "/" + path.split(".")[0]
        return name

    async def get_remote_version(self) -> str:
        return url_version(self.url)
