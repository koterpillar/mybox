from collections.abc import AsyncIterator, Awaitable, Callable
from contextlib import AbstractContextManager, asynccontextmanager
from dataclasses import dataclass
from functools import partial
from typing import Generic, Optional, Protocol

import trio
from alive_progress import alive_bar  # type: ignore

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


class ProgressBar(Protocol):
    def pause(self) -> AbstractContextManager[None]: ...
    def __call__(self) -> None: ...


CURRENT_PROGRESS: Optional[ProgressBar] = None


async def parallel_map_progress(
    action: Callable[[T], Awaitable[U]], items: list[T]
) -> list[U]:
    with alive_bar(len(items)) as progress:
        global CURRENT_PROGRESS  # pylint:disable=global-statement
        async with TERMINAL_LOCK:
            CURRENT_PROGRESS = progress

        try:

            async def action_and_update(item: T) -> U:
                result = await action(item)
                async with TERMINAL_LOCK:
                    if CURRENT_PROGRESS is not None:
                        CURRENT_PROGRESS()
                return result

            return await gather(*(partial(action_and_update, item) for item in items))
        finally:
            async with TERMINAL_LOCK:
                CURRENT_PROGRESS = None


@asynccontextmanager
async def parallel_map_pause() -> AsyncIterator[None]:
    async with TERMINAL_LOCK:
        if CURRENT_PROGRESS is None:
            yield
        else:
            with CURRENT_PROGRESS.pause():
                yield


async def gather(*tasks: Callable[[], Awaitable[T]]) -> list[T]:
    results: dict[int, PartialResult[T]] = {}

    async with trio.open_nursery() as nursery:

        async def collect(index: int, task: Callable[[], Awaitable[T]]):
            try:
                results[index] = PartialSuccess(result=await task())
            except Exception as e:  # pylint:disable=broad-exception-caught
                results[index] = PartialException(exception=e)
            except BaseException as e:  # pylint:disable=broad-exception-caught
                results[index] = PartialException(exception=e)
                nursery.cancel_scope.cancel()

        for index, task in enumerate(tasks):
            nursery.start_soon(collect, index, task)

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
