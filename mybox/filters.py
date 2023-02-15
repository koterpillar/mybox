import re
from typing import Callable, Iterator

from .utils import Some, T, unsome


class Filters:
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

    prefixes: list[str]
    suffixes: list[str]
    includes: list[str]
    excludes: list[str]
    regex: list[str]

    def __init__(
        self,
        *,
        prefix: Some[str] = None,
        suffix: Some[str] = None,
        include: Some[str] = None,
        exclude: Some[str] = None,
        regex: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.prefixes = unsome(prefix)
        self.suffixes = unsome(suffix)
        self.includes = unsome(include)
        self.excludes = unsome(exclude)
        self.regex = unsome(regex)

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
