import subprocess
from functools import wraps
from pathlib import Path
from typing import Any, Callable, Coroutine, Iterable, Optional, TypeVar, overload

import requests
import trio
from pydantic import validator

T = TypeVar("T")
U = TypeVar("U")
V = TypeVar("V")


Some = Optional[T | list[T]]


def unsome_(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


def unsome(x: Some[T]) -> list[T]:
    return unsome_(x) or []


def allow_singular(field: str) -> Any:
    return validator(field, pre=True, allow_reuse=True)(lambda cls, x: unsome_(x))


def allow_singular_none(field: str) -> Any:
    return validator(field, pre=True, allow_reuse=True)(lambda cls, x: unsome(x))


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


RunArg = str | Path


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
