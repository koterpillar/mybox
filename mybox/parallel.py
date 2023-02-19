from dataclasses import dataclass
from functools import partial
from typing import Awaitable, Callable, Generic, Union

import tqdm
import trio

from .utils import TERMINAL_LOCK, T, U, raise_


@dataclass
class PartialSuccess(Generic[T]):
    result: T


@dataclass
class PartialException:
    exception: BaseException


PartialResult = Union[PartialException, PartialSuccess[T]]


class PartialResults(Exception, Generic[T]):
    def __init__(self, results: list[PartialResult[T]]):
        self.results = results


async def parallel_map_tqdm(
    action: Callable[[T], Awaitable[U]], items: list[T]
) -> list[U]:
    with tqdm.tqdm(total=len(items)) as progress:

        async def action_and_update(item: T) -> U:
            result = await action(item)
            async with TERMINAL_LOCK:
                progress.update(1)
            return result

        return await gather_(*(partial(action_and_update, item) for item in items))


async def gather(*tasks: Callable[[], Awaitable[T]]) -> list[PartialResult[T]]:
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
    return [results[i] for i in range(len(tasks))]


async def gather_(*tasks: Callable[[], Awaitable[T]]) -> list[T]:
    partial_results = await gather(*tasks)

    return [
        result.result
        if isinstance(result, PartialSuccess)
        else raise_(PartialResults(partial_results))
        for result in partial_results
    ]
