from urllib.parse import urlparse

from pydantic import Field

from ..compute import Const, Value
from ..utils import async_cached, url_version
from .archive import ArchivePackage


class URLPackage(ArchivePackage):
    url_: Value = Field(..., alias="url")

    @async_cached
    async def url(self) -> str:
        return await self.url_.compute()

    async def archive_url(self) -> str:
        return await self.url()

    def derive_name(self) -> str:
        if isinstance(self.url_, Const):
            url_str = self.url_.value
        else:
            raise ValueError(
                "Cannot derive name from computed URL and no name override."
            )

        url = urlparse(url_str)

        name = url.hostname or ""

        path = url.path.rsplit("/", 1)[-1]
        if path:
            name += "/" + path.split(".")[0]
        return name

    async def get_remote_version(self) -> str:
        return url_version(await self.url())
