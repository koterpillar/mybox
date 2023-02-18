from functools import partial
from typing import Awaitable, Callable

import tqdm  # type: ignore
import trio

from .utils import TERMINAL_LOCK, T, U


async def parallel_map_tqdm(
    action: Callable[[T], Awaitable[U]], items: list[T]
) -> list[U]:
    with tqdm.tqdm(total=len(items)) as progress:

        async def action_and_update(item: T) -> U:
            result = await action(item)
            async with TERMINAL_LOCK:
                progress.update(1)
            return result

        return await gather(*(partial(action_and_update, item) for item in items))


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
