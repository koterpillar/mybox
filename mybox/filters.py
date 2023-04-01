import re
from typing import Callable, Iterator

from pydantic import BaseModel, Field

from .utils import T, allow_singular_none


class Filters(BaseModel):
    @staticmethod
    def includes_(substring: str) -> Callable[[str], bool]:
        return lambda x: substring in x.lower()

    @staticmethod
    def excludes_(substring: str) -> Callable[[str], bool]:
        return lambda x: substring not in x

    @staticmethod
    def startswith(prefix: str) -> Callable[[str], bool]:
        return lambda x: x.startswith(prefix)

    @staticmethod
    def endswith(suffix: str) -> Callable[[str], bool]:
        return lambda x: x.endswith(suffix)

    @staticmethod
    def regex_(regex: str) -> Callable[[str], bool]:
        regex_compiled = re.compile(regex)

        return lambda x: regex_compiled.match(x) is not None

    prefixes: list[str] = Field(default_factory=list, alias="prefix")
    prefixes_val = allow_singular_none("prefixes")

    suffixes: list[str] = Field(default_factory=list, alias="suffix")
    suffixes_val = allow_singular_none("suffixes")

    includes: list[str] = Field(default_factory=list, alias="include")
    includes_val = allow_singular_none("includes")

    excludes: list[str] = Field(default_factory=list, alias="exclude")
    excludes_val = allow_singular_none("excludes")

    regex: list[str] = Field(default_factory=list)
    regex_val = allow_singular_none("regex")

    def filters(self) -> Iterator[Callable[[str], bool]]:
        for prefix in self.prefixes:
            yield self.startswith(prefix)
        for suffix in self.suffixes:
            yield self.endswith(suffix)
        for include in self.includes:
            yield self.includes_(include)
        for exclude in self.excludes:
            yield self.excludes_(exclude)
        for regex in self.regex:
            yield self.regex_(regex)


def choose(candidates: list[T], filters: Iterator[Callable[[T], bool]]) -> T:
    if len(candidates) == 0:
        raise ValueError("No candidates to choose from.")
    while len(candidates) > 1:
        try:
            filter_fn = next(filters)
        except StopIteration:
            break

        new_candidates = list(filter(filter_fn, candidates))
        if new_candidates:
            candidates = new_candidates

    if len(candidates) == 1:
        return candidates[0]

    raise ValueError(f"Cannot choose between: {candidates}.")
