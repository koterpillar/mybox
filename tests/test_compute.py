import json
from typing import Any

import pytest

from mybox.compute import Const, JSONPath, Value
from mybox.utils import T


class TestParse:
    def parse_as(self, klass: type[T], value: Any) -> T:
        result = Value.parse(value)

        assert isinstance(result, klass)
        return result

    @pytest.mark.trio
    async def test_static(self):
        value = self.parse_as(Const, "example")

        assert (await value.compute()) == "example"

    @pytest.mark.trio
    async def test_jsonpath(self):
        value = self.parse_as(
            JSONPath,
            {"base": r"{'json': true}", "jsonpath": "foo[*].bar", "include": "nice"},
        )

        assert (await value.base.compute()) == r"{'json': true}"

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
