import re
import subprocess
from functools import wraps
from pathlib import Path
from typing import (
    Any,
    Awaitable,
    Callable,
    Coroutine,
    Iterable,
    Iterator,
    Optional,
    TypeVar,
    Union,
    overload,
)

import requests
import tqdm  # type: ignore
import trio

T = TypeVar("T")
U = TypeVar("U")
V = TypeVar("V")


Some = Optional[Union[T, list[T]]]


def unsome_(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


def unsome(x: Some[T]) -> list[T]:
    return unsome_(x) or []


def intercalate(delimiter: T, items: Iterable[Iterable[T]]) -> list[T]:
    it = iter(items)
    try:
        result = list(next(it))
    except StopIteration:
        return []
    for x in it:
        result.append(delimiter)
        result.extend(list(x))
    return result


TERMINAL_LOCK = trio.Lock()


RunArg = Union[str, Path]


async def run(*args: RunArg) -> subprocess.CompletedProcess:
    return await trio.run_process(args, check=True)


async def run_ok(*args: RunArg) -> bool:
    try:
        result = await trio.run_process(
            args, check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
        return result.returncode == 0
    except FileNotFoundError:
        return False


async def run_output(*args: RunArg, silent: bool = False) -> str:
    result = await trio.run_process(
        args, capture_stdout=True, capture_stderr=silent, check=True
    )
    return result.stdout.decode().strip()


def flatten(items: Iterable[Iterable[T]]) -> list[T]:
    return [item for sublist in items for item in sublist]


async def parallel_map_tqdm(
    action: Callable[[T], Awaitable[U]], items: list[T]
) -> list[U]:
    results: list[U] = []

    with tqdm.tqdm(total=len(items)) as progress:
        async with trio.open_nursery() as nursery:

            async def action_and_update(item: T) -> None:
                result = await action(item)
                async with TERMINAL_LOCK:
                    progress.update(1)
                results.append(result)

            for item in items:
                nursery.start_soon(action_and_update, item)

            return results


async def gather(*tasks: Callable[[], Awaitable[T]]) -> list[T]:
    async def collect(
        index: int, task: Callable[[], Awaitable[T]], results: dict[int, T]
    ):
        results[index] = await task()

    results: dict[int, T] = {}
    async with trio.open_nursery() as nursery:
        for index, task in enumerate(tasks):
            nursery.start_soon(collect, index, task, results)
    return [results[i] for i in range(len(tasks))]


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


AsyncRet = Coroutine[Any, Any, T]


@overload
def async_cached(fn: Callable[[], AsyncRet[T]]) -> Callable[[], AsyncRet[T]]:
    ...


@overload
def async_cached(fn: Callable[[U], AsyncRet[T]]) -> Callable[[U], AsyncRet[T]]:
    ...


@overload
def async_cached(fn: Callable[[U, V], AsyncRet[T]]) -> Callable[[U, V], AsyncRet[T]]:
    ...


def async_cached(fn: Callable[..., AsyncRet[T]]) -> Callable[..., AsyncRet[T]]:
    return _async_cached_lock(None, fn)


@overload
def async_cached_lock(fn: Callable[[], AsyncRet[T]]) -> Callable[[], AsyncRet[T]]:
    ...


@overload
def async_cached_lock(fn: Callable[[U], AsyncRet[T]]) -> Callable[[U], AsyncRet[T]]:
    ...


@overload
def async_cached_lock(
    fn: Callable[[U, V], AsyncRet[T]]
) -> Callable[[U, V], AsyncRet[T]]:
    ...


def async_cached_lock(fn: Callable[..., AsyncRet[T]]) -> Callable[..., AsyncRet[T]]:
    return _async_cached_lock(trio.Lock(), fn)


class NoLock:
    async def __aenter__(self) -> None:
        pass

    async def __aexit__(self, *args: Any) -> None:
        pass


NO_LOCK = NoLock()


def _async_cached_lock(
    lock: Optional[trio.Lock], fn: Callable[..., AsyncRet[T]]
) -> Callable[..., AsyncRet[T]]:
    cache: dict[Any, T] = {}

    @wraps(fn)
    async def cached_fn(*args: Any) -> T:
        if args not in cache:
            async with lock or NO_LOCK:
                # Check again, someone holding the lock might have updated the cache
                if args not in cache:
                    cache[args] = await fn(*args)
        return cache[args]

    return cached_fn
