from typing import Any

import pytest

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
