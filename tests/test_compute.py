import json
from typing import Any, cast

import pytest

from mybox.compute import URL, Const, Format, HTMLLinks, JSONPath, Value
from mybox.utils import T


class TestParse:
    def parse_as(self, klass: type[T], value: Any) -> T:
        result = Value.parse(value)

        assert isinstance(result, klass)
        return result

    def test_static(self):
        value = self.parse_as(Const, "example")

        assert value.value_ == "example"

    def test_url(self):
        value = self.parse_as(URL, {"url": "https://example.com"})

        assert cast(Const, value.base).value_ == "https://example.com"

    def test_format(self):
        value = self.parse_as(Format, {"base": "foo", "format": r"bar {}"})

        assert value.format == r"bar {}"

    def test_jsonpath(self):
        value = self.parse_as(
            JSONPath,
            {"base": r"{'json': true}", "jsonpath": "foo[*].bar", "include": "nice"},
        )

        assert cast(Const, value.base).value_ == r"{'json': true}"
        assert str(value.jsonpath) == "foo.[*].bar"

    def test_links(self):
        value = self.parse_as(HTMLLinks, {"links": r"<html></html>"})

        assert cast(Const, value.base).value_ == r"<html></html>"

    def test_errors(self):
        with pytest.raises(ValueError):
            Value.parse(123)
        with pytest.raises(ValueError):
            Value.parse([])
        with pytest.raises(ValueError):
            Value.parse({})
        with pytest.raises(ValueError):
            Value.parse({"nonsense": "foo"})


@pytest.mark.trio
async def test_jsonpath():
    value = Value.parse(
        {
            "base": json.dumps({"foo": [{"bar": "aaaa"}, {"bar": "bbbb"}]}),
            "jsonpath": "foo[*].bar",
            "exclude": "bbbb",
        }
    )

    assert (await value.compute()) == "aaaa"


@pytest.mark.trio
async def test_url():
    value = Value.parse({"url": "https://httpbin.org/json"})

    assert "slideshow" in (await value.compute())


@pytest.mark.trio
async def test_links():
    value = Value.parse(
        {"links": "<html><a href='https://example.com'>example</a></html>"}
    )

    assert (await value.compute()) == "https://example.com"


@pytest.mark.trio
async def test_format():
    value = Value.parse({"base": "foo", "format": "bar {}"})

    assert (await value.compute()) == "bar foo"
