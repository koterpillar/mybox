import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from typing import Any, cast

import pytest
import trio

from mybox.compute import URL, Const, Format, HTMLLinks, JSONPath, parse_value
from mybox.utils import T


class TestParse:
    def parse_as(self, klass: type[T], value: Any) -> T:
        result = parse_value(value)

        assert isinstance(result, klass)
        return result

    def test_static(self):
        value = self.parse_as(Const, "example")

        assert value.value == "example"

    def test_url(self):
        value = self.parse_as(URL, {"url": "https://example.com"})

        assert cast(Const, value.base).value == "https://example.com"

    def test_format(self):
        value = self.parse_as(Format, {"base": "foo", "format": r"bar {}"})

        assert value.format == r"bar {}"

    def test_jsonpath(self):
        value = self.parse_as(
            JSONPath,
            {"base": r"{'json': true}", "jsonpath": "foo[*].bar", "include": "nice"},
        )

        assert cast(Const, value.base).value == r"{'json': true}"
        assert str(value.jsonpath) == "foo.[*].bar"

    def test_links(self):
        value = self.parse_as(HTMLLinks, {"links": r"<html></html>"})

        assert cast(Const, value.base).value == r"<html></html>"

    def test_errors(self):
        with pytest.raises(ValueError):
            parse_value(123)
        with pytest.raises(ValueError):
            parse_value([])
        with pytest.raises(ValueError):
            parse_value({})
        with pytest.raises(ValueError):
            parse_value({"nonsense": "foo"})


@pytest.mark.trio
async def test_jsonpath():
    value = parse_value(
        {
            "base": json.dumps({"foo": [{"bar": "aaaa"}, {"bar": "bbbb"}]}),
            "jsonpath": "foo[*].bar",
            "exclude": "bbbb",
        }
    )

    assert (await value.compute()) == "aaaa"


@pytest.mark.trio
async def test_jsonpath_filter():
    value = parse_value(
        {
            "base": json.dumps(
                {
                    "foo": [
                        {"bar": "aaaa", "result": "one"},
                        {"bar": "bbbb", "result": "two"},
                    ]
                }
            ),
            "jsonpath": "$.foo[?(@.bar=='aaaa')].result",
        }
    )

    assert (await value.compute()) == "one"


@pytest.mark.trio
async def test_url():
    class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            self.wfile.write(b"Hello World")

    server = HTTPServer(("localhost", 0), SimpleHTTPRequestHandler)

    trio.lowlevel.start_thread_soon(server.serve_forever, lambda result: None)

    value = parse_value({"url": f"http://localhost:{server.server_port}"})

    assert await value.compute() == "Hello World"


@pytest.mark.trio
async def test_links():
    value = parse_value(
        {"links": "<html><a href='https://example.com'>example</a></html>"}
    )

    assert (await value.compute()) == "https://example.com"


@pytest.mark.trio
async def test_format():
    value = parse_value({"base": "foo", "format": "bar {}"})

    assert (await value.compute()) == "bar foo"
