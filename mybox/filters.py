import re
from typing import Callable, Iterator

from .utils import T


class Filters:
    @staticmethod
    def includes(substring: str) -> Callable[[str], bool]:
        return lambda x: substring in x.lower()

    @staticmethod
    def excludes(substring: str) -> Callable[[str], bool]:
        return lambda x: substring not in x

    @staticmethod
    def startswith(prefix: str) -> Callable[[str], bool]:
        return lambda x: x.startswith(prefix)

    @staticmethod
    def endswith(suffix: str) -> Callable[[str], bool]:
        return lambda x: x.endswith(suffix)

    @staticmethod
    def regex(regex: str) -> Callable[[str], bool]:
        regex_compiled = re.compile(regex)

        return lambda x: regex_compiled.match(x) is not None


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
