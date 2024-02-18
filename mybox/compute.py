import json
from abc import ABC, abstractmethod
from typing import Annotated, Any

import requests
from bs4 import BeautifulSoup
from jsonpath_ng import JSONPath as JSONPathT  # type: ignore
from jsonpath_ng.ext import parse as jsonpath_parse  # type: ignore
from pydantic import BaseModel, ConfigDict, field_validator
from pydantic.functional_validators import BeforeValidator

from .filters import Filters, choose


class ValueC(BaseModel, ABC):
    @abstractmethod
    async def compute(self) -> str:
        raise NotImplementedError


def parse_value(value: Any) -> ValueC:
    if isinstance(value, str):
        return Const(value=value)
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


Value = Annotated[ValueC, BeforeValidator(parse_value)]


class Const(ValueC):
    value: str

    async def compute(self):
        return self.value


class Derived(ValueC, ABC):
    base: Value

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
    async def derived_value(self, contents: str) -> str:
        soup = BeautifulSoup(contents, "html.parser")
        candidates: list[str] = list(
            filter(None, (link.get("href") for link in soup.find_all("a")))
        )
        return choose(candidates, self.filters())
