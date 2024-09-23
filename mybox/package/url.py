from urllib.parse import urlparse

from pydantic import Field

from ..compute import Value, compute
from ..utils import async_cached, url_version
from .archive import ArchivePackage


class URLPackage(ArchivePackage):
    url_: Value = Field(..., alias="url")

    @async_cached
    async def url(self) -> str:
        return await compute(self.url_)

    async def archive_url(self) -> str:
        return await self.url()

    def derive_name(self) -> str:
        if isinstance(self.url_, str):
            url_str = self.url_
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
        return await url_version(await self.url())
