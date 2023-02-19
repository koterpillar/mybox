import json
from abc import ABCMeta, abstractmethod
from typing import Any

import requests
from bs4 import BeautifulSoup
from jsonpath_ng import parse as jsonpath_parse  # type: ignore

from .filters import Filters, choose


class Value(metaclass=ABCMeta):
    @staticmethod
    def parse(value: Any) -> "Value":
        if isinstance(value, str):
            return Const(value)
        if isinstance(value, dict):
            if base := value.pop("url", None):
                return URL(base=base)
            if base := value.pop("links", None):
                return HTMLLinks(base=base, **value)
            if "format" in value:
                return Format(**value)
            if "jsonpath" in value:
                return JSONPath(**value)
        raise ValueError(f"Cannot parse URL from {value!r}.")

    @abstractmethod
    async def compute(self) -> str:
        raise NotImplementedError


class Const(Value):
    def __init__(self, value):
        self.value_ = value

    async def compute(self):
        return self.value_


class Derived(Value, metaclass=ABCMeta):
    base: Value

    def __init__(self, *, base: Any, **kwargs):
        self.base = Value.parse(base)
        super().__init__(**kwargs)

    @abstractmethod
    async def derived_value(self, contents: str) -> str:
        raise NotImplementedError

    async def compute(self) -> str:
        base = await self.base.compute()
        return await self.derived_value(base)


class URL(Derived):
    async def derived_value(self, contents: str) -> str:
        return requests.get(contents).text


class JSONPath(Derived, Filters):
    def __init__(self, *, jsonpath: str, **kwargs):
        self.jsonpath = jsonpath_parse(jsonpath)
        super().__init__(**kwargs)

    async def derived_value(self, contents: str) -> str:
        json_contents = json.loads(contents)
        candidates = [
            candidate.value for candidate in self.jsonpath.find(json_contents)
        ]
        return choose(candidates, self.filters())


class Format(Derived):
    def __init__(self, *, format: str, **kwargs):  # pylint:disable=redefined-builtin
        self.format = format
        super().__init__(**kwargs)

    async def derived_value(self, contents: str) -> str:
        return self.format.format(contents)


class HTMLLinks(Derived, Filters):
    async def derived_value(self, contents: str) -> str:
        soup = BeautifulSoup(contents, "html.parser")
        candidates = [link.get("href") for link in soup.find_all("a")]
        return choose(candidates, self.filters())
