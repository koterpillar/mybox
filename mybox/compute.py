import json
from abc import ABC, abstractmethod
from typing import Any, Union

from bs4 import BeautifulSoup
from jsonpath_ng import JSONPath as JSONPathT  # type: ignore
from jsonpath_ng.ext import parse as jsonpath_parse  # type: ignore
from pydantic import BaseModel, ConfigDict, Field, TypeAdapter, field_validator

from .filters import Filters, choose
from .utils import http_get


class ValueC(BaseModel, ABC):
    model_config = ConfigDict(frozen=True, extra="forbid")

    @abstractmethod
    async def compute(self) -> str:
        raise NotImplementedError


async def compute(value: Union[str, ValueC]) -> str:
    if isinstance(value, str):
        return value
    return await value.compute()


class Derived(ValueC, ABC):
    base: "Value"

    @abstractmethod
    async def derived_value(self, contents: str) -> str:
        raise NotImplementedError

    async def compute(self) -> str:
        base = await compute(self.base)
        return await self.derived_value(base)


class URL(Derived):
    base: "Value" = Field(..., alias="url")

    async def derived_value(self, contents: str) -> str:
        return await http_get(contents)


class JSONPath(Derived, Filters):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    jsonpath: JSONPathT

    @field_validator("jsonpath", mode="before")
    @classmethod
    def parse_jsonpath(cls, value):
        return jsonpath_parse(value)

    async def derived_value(self, contents: str) -> str:
        json_contents = json.loads(contents)
        candidates = [
            candidate.value for candidate in self.jsonpath.find(json_contents)
        ]
        return choose(candidates, self.filters())


class Format(Derived):
    format: str  # pylint:disable=redefined-builtin

    async def derived_value(self, contents: str) -> str:
        return self.format.format(contents)


class HTMLLinks(Derived, Filters):
    base: "Value" = Field(..., alias="links")

    async def derived_value(self, contents: str) -> str:
        soup = BeautifulSoup(contents, "html.parser")
        candidates: list[str] = list(
            filter(None, (link.get("href") for link in soup.find_all("a")))
        )
        return choose(candidates, self.filters())


Value = Union[str, URL, HTMLLinks, Format, JSONPath]


ValueAdapter: TypeAdapter[Value] = TypeAdapter(Value)


def parse_value(value: Any) -> Value:
    return ValueAdapter.validate_python(value)
