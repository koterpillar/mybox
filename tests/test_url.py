from typing import Any

import pytest
from packaging.version import parse as parse_version

from mybox.url import URL, JSONPath, Static
from mybox.utils import T


class TestParse:
    def parse_as(self, klass: type[T], value: Any) -> T:
        url = URL.parse(value)

        assert isinstance(url, klass)
        return url

    @pytest.mark.trio
    async def test_static(self):
        url = self.parse_as(Static, "http://example.com")

        assert (await url.value()) == "http://example.com"

    @pytest.mark.trio
    async def test_jsonpath(self):
        url = self.parse_as(
            JSONPath,
            {"url": "http://example.com", "jsonpath": "foo[*].bar", "include": "nice"},
        )

        assert (await url.url.value()) == "http://example.com"

    def test_errors(self):
        with pytest.raises(ValueError):
            URL.parse(123)
        with pytest.raises(ValueError):
            URL.parse([])
        with pytest.raises(ValueError):
            URL.parse({})
        with pytest.raises(ValueError):
            URL.parse({"nonsense": "foo"})


@pytest.mark.trio
async def test_jsonpath():
    zoom = URL.parse(
        {
            "url": "https://zoom.us/rest/download?os=linux",
            "jsonpath": "result.downloadVO.zoom.version",
        }
    )

    url = parse_version(await zoom.value())

    assert parse_version("5.13") <= url <= parse_version("10.0")
