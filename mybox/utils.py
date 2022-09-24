import concurrent.futures
import re
import subprocess
from pathlib import Path
from threading import Lock
from typing import Any, Callable, Iterable, Iterator, Optional, TypeVar, Union

import requests
import tqdm  # type: ignore

T = TypeVar("T")
U = TypeVar("U")


Some = Optional[Union[T, list[T]]]


def unsome_(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


def unsome(x: Some[T]) -> list[T]:
    return unsome_(x) or []


TERMINAL_LOCK = Lock()


def run(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run(args, check=True)


def run_ok(*args: str) -> bool:
    return (
        subprocess.run(
            args,
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        ).returncode
        == 0
    )


def run_output(*args: str) -> str:
    return (
        subprocess.run(args, stdout=subprocess.PIPE, check=True).stdout.decode().strip()
    )


def flatten(items: Iterable[Iterable[T]]) -> list[T]:
    return [item for sublist in items for item in sublist]


def parallel_map_tqdm(action: Callable[[T], U], items: list[T]) -> list[U]:
    with tqdm.tqdm(total=len(items)) as progress:
        with concurrent.futures.ThreadPoolExecutor(20) as executor:

            def action_and_update(item: T) -> U:
                result = action(item)
                with TERMINAL_LOCK:
                    progress.update(1)
                return result

            return list(executor.map(action_and_update, items))


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


def url_version(url: str) -> str:
    head_response = requests.head(url, allow_redirects=True)
    return head_response.headers["etag"]


def raise_(exception: BaseException) -> Any:
    raise exception


def transplant_path(dir_from: Path, dir_to: Path, path: Path) -> Path:
    return dir_to.joinpath(path.relative_to(dir_from))
