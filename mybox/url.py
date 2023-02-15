import json
from abc import ABCMeta, abstractmethod
from typing import Any

import requests
from jsonpath_ng import parse as jsonpath_parse  # type: ignore

from .filters import Filters, choose


class URL(metaclass=ABCMeta):
    @staticmethod
    def parse(value: Any) -> "URL":
        if isinstance(value, str):
            return Static(value)
        if isinstance(value, dict):
            if "jsonpath" in value:
                return JSONPath(**value)
        raise ValueError(f"Cannot parse URL from {value!r}.")

    @abstractmethod
    async def value(self) -> str:
        raise NotImplementedError


class Static(URL):
    def __init__(self, value):
        self.value_ = value

    async def value(self):
        return self.value_


class Derived(URL, metaclass=ABCMeta):
    url: URL

    def __init__(self, *, url: Any, **kwargs):
        self.url = URL.parse(url)
        super().__init__(**kwargs)

    @abstractmethod
    async def derived_value(self, contents: str) -> str:
        raise NotImplementedError

    async def value(self) -> str:
        base = await self.url.value()
        contents = requests.get(base)
        return await self.derived_value(contents.text)


class JSONPath(Derived, Filters):
    def __init__(self, *, jsonpath: str, **kwargs):
        self.jsonpath = jsonpath_parse(jsonpath)
        super().__init__(**kwargs)

    async def derived_value(self, contents: str) -> str:
        json_contents = json.loads(contents)
        candidates = self.jsonpath.find(json_contents)
        return choose(candidates, self.filters())
