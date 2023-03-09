import pytest

from mybox.parallel import (
    PartialException,
    PartialResult,
    PartialResults,
    parallel_map_pause,
    parallel_map_tqdm,
)
from mybox.utils import T


class TestParallelMapTqdm:
    @staticmethod
    async def alen(item: str) -> int:
        length = len(item)
        if length > 5:
            raise ValueError("too long")
        return length

    @pytest.mark.trio
    async def test_success(self):
        results = await parallel_map_tqdm(self.alen, ["one", "two", "three", "four"])
        assert results == [3, 3, 5, 4]

    @staticmethod
    def display_result(result: PartialResult[T]) -> str:
        if isinstance(result, PartialException):
            return f"exception: {result.exception}"
        return f"success: {result.result}"

    @pytest.mark.trio
    async def test_exceptions(self):
        with pytest.raises(PartialResults) as excinfo:
            await parallel_map_tqdm(
                self.alen, ["one", "two", "twelve", "three", "four", "eleven"]
            )

        assert [self.display_result(result) for result in excinfo.value.results] == [
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
        results = await parallel_map_tqdm(self.paused_len, ["one", "two", "three"])
        assert results == [3, 3, 5]
