from contextlib import asynccontextmanager
from dataclasses import dataclass
from functools import partial
from typing import AsyncIterator, Awaitable, Callable, Generic, Optional

import tqdm
import trio

from .utils import T, U, raise_


@dataclass
class PartialSuccess(Generic[T]):
    result: T


@dataclass
class PartialException:
    exception: BaseException


PartialResult = PartialException | PartialSuccess[T]


@dataclass
class PartialResults(Exception, Generic[T]):
    results: list[PartialResult[T]]


TERMINAL_LOCK = trio.Lock()

CURRENT_TQDM: Optional[tqdm.tqdm] = None


async def parallel_map_tqdm(
    action: Callable[[T], Awaitable[U]], items: list[T]
) -> list[U]:
    with tqdm.tqdm(total=len(items)) as progress:
        global CURRENT_TQDM  # pylint:disable=global-statement
        async with TERMINAL_LOCK:
            CURRENT_TQDM = progress
        try:

            async def action_and_update(item: T) -> U:
                result = await action(item)
                async with TERMINAL_LOCK:
                    progress.update(1)
                return result

            return await gather(*(partial(action_and_update, item) for item in items))
        finally:
            async with TERMINAL_LOCK:
                CURRENT_TQDM = None


@asynccontextmanager
async def parallel_map_pause() -> AsyncIterator[None]:
    async with TERMINAL_LOCK:
        progress = CURRENT_TQDM
        if progress:
            progress.clear()
        try:
            yield
        finally:
            if progress:
                progress.refresh()


async def gather(*tasks: Callable[[], Awaitable[T]]) -> list[T]:
    async def collect(
        index: int,
        task: Callable[[], Awaitable[T]],
        results: dict[int, PartialResult[T]],
    ):
        try:
            results[index] = PartialSuccess(result=await task())
        except BaseException as e:  # pylint:disable=broad-exception-caught
            results[index] = PartialException(exception=e)

    results: dict[int, PartialResult[T]] = {}

    async with trio.open_nursery() as nursery:
        for index, task in enumerate(tasks):
            nursery.start_soon(collect, index, task, results)

    partial_results = [results[i] for i in range(len(tasks))]

    return [
        (
            result.result
            if isinstance(result, PartialSuccess)
            else raise_(PartialResults(partial_results))
        )
        for result in partial_results
    ]


async def gather_(*tasks: Callable[[], Awaitable[T]]) -> list[T]:
    try:
        return await gather(*tasks)
    except PartialResults as exc:
        for result in exc.results:
            if isinstance(result, PartialException):
                raise result.exception from None
        # Must have at least one exception inside PartialResults
        raise AssertionError("PartialResults is empty") from None  # pragma: no cover
