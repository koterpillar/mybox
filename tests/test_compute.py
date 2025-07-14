import json
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from http.server import BaseHTTPRequestHandler, HTTPServer
from typing import Any

import pytest
import trio

from mybox.compute import URL, Format, HTMLLinks, JSONPath, compute, parse_value
from mybox.utils import T


@asynccontextmanager
async def http_test_server(page_contents: str) -> AsyncIterator[str]:
    class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            self.wfile.write(page_contents.encode())

    server = HTTPServer(("localhost", 0), SimpleHTTPRequestHandler)
    port = server.server_port

    # Start server in a separate thread
    trio.lowlevel.start_thread_soon(server.serve_forever, lambda result: None)

    try:
        yield f"http://localhost:{port}"
    finally:
        server.shutdown()
        server.server_close()


class TestParse:
    def parse_as(self, klass: type[T], value: Any) -> T:
        result = parse_value(value)

        assert isinstance(result, klass)
        return result

    def test_static(self):
        value = self.parse_as(str, "example")

        assert value == "example"

    def test_url(self):
        value = self.parse_as(URL, {"url": "https://example.com"})

        assert value.base == "https://example.com"

    def test_format(self):
        value = self.parse_as(Format, {"base": "foo", "format": r"bar {}"})

        assert value.format == r"bar {}"

    def test_jsonpath(self):
        value = self.parse_as(
            JSONPath,
            {"base": r"{'json': true}", "jsonpath": "foo[*].bar", "include": "nice"},
        )

        assert value.base == r"{'json': true}"
        assert str(value.jsonpath) == "foo.[*].bar"

    def test_links(self):
        value = self.parse_as(HTMLLinks, {"links": r"<html></html>"})

        assert value.base == r"<html></html>"

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

    assert (await compute(value)) == "aaaa"


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

    assert (await compute(value)) == "one"


@pytest.mark.trio
async def test_url():
    async with http_test_server("Hello World") as url:
        value = parse_value({"url": url})

        assert await compute(value) == "Hello World"


LINKS_HTML = """
<html>
    <a href='https://example.com'>example</a>
    <a href='/relative'>relative</a>
</html>
"""


@pytest.mark.trio
async def test_links():
    async with http_test_server(LINKS_HTML) as url:
        value = parse_value({"links": url, "include": "example"})

        assert (await compute(value)) == "https://example.com"


@pytest.mark.trio
async def test_links_relative():
    async with http_test_server(LINKS_HTML) as url:
        value = parse_value({"links": url, "include": "relative"})

        assert (await compute(value)) == f"{url}/relative"


@pytest.mark.trio
async def test_format():
    value = parse_value({"base": "foo", "format": "bar {}"})

    assert (await compute(value)) == "bar foo"
