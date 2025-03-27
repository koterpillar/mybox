from functools import partial

import pytest
import trio

from mybox.parallel import (
    PartialException,
    PartialResult,
    PartialResults,
    gather,
    gather_,
    parallel_map_pause,
    parallel_map_progress,
)
from mybox.utils import T


async def alen(item: str) -> int:
    length = len(item)
    if length > 5:
        raise ValueError("too long")
    return length


async def sleep_and_error(duration: float) -> int:
    await trio.sleep(duration)
    # The sleep is expected to be interrupted by keyboard_interrupt
    raise ValueError("slept for the whole duration")  # pragma: no cover


async def keyboard_interrupt(duration: float) -> int:
    await trio.sleep(duration)
    raise KeyboardInterrupt("interrupted")


def display_result(result: PartialResult[T]) -> str:
    if isinstance(result, PartialException):
        return f"exception: {result.exception}"
    return f"success: {result.result}"


class TestGather:
    @pytest.mark.trio
    @pytest.mark.parametrize("gather_fn", [gather, gather_])
    async def test_success(self, gather_fn):
        results = await gather_fn(
            partial(alen, "one"), partial(alen, "two"), partial(alen, "three")
        )
        assert results == [3, 3, 5]

    @pytest.mark.trio
    async def test_failures(self):
        with pytest.raises(PartialResults) as excinfo:
            await gather(
                partial(alen, "one"),
                partial(alen, "eleven"),
                partial(alen, "two"),
                partial(alen, "twelve"),
                partial(alen, "three"),
            )

        assert [display_result(result) for result in excinfo.value.results] == [
            "success: 3",
            "exception: too long",
            "success: 3",
            "exception: too long",
            "success: 5",
        ]

    @pytest.mark.trio
    async def test_keyboard_interrupt(self):
        with pytest.raises(PartialResults) as excinfo:
            await gather(
                partial(alen, "one"),
                partial(keyboard_interrupt, 0.2),
                partial(alen, "two"),
                partial(sleep_and_error, 2),
                partial(alen, "three"),
            )

        assert [display_result(result) for result in excinfo.value.results] == [
            "success: 3",
            "exception: interrupted",
            "success: 3",
            "exception: Cancelled",
            "success: 5",
        ]

    @pytest.mark.trio
    async def test_single_failure(self):
        with pytest.raises(ValueError) as excinfo:
            await gather_(
                partial(alen, "one"),
                partial(alen, "eleven"),
                partial(alen, "two"),
                partial(alen, "twelve"),
                partial(alen, "three"),
            )
        assert excinfo.value.args == ("too long",)


class TestParallelMapProgress:
    @pytest.mark.trio
    async def test_success(self):
        results = await parallel_map_progress(alen, ["one", "two", "three", "four"])
        assert results == [3, 3, 5, 4]

    @pytest.mark.trio
    async def test_exceptions(self):
        with pytest.raises(PartialResults) as excinfo:
            await parallel_map_progress(
                alen, ["one", "two", "twelve", "three", "four", "eleven"]
            )

        assert [display_result(result) for result in excinfo.value.results] == [
            "success: 3",
            "success: 3",
            "exception: too long",
            "success: 5",
            "success: 4",
            "exception: too long",
        ]


class TestParallelMapPause:
    @staticmethod
    async def paused_len(item: str) -> int:
        async with parallel_map_pause():
            return len(item)

    @pytest.mark.trio
    async def test_outside(self):
        result = await self.paused_len("one")
        assert result == 3

    @pytest.mark.trio
    async def test_inside_map(self):
        results = await parallel_map_progress(self.paused_len, ["one", "two", "three"])
        assert results == [3, 3, 5]
