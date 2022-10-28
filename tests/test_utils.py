import re

import pytest

from mybox.utils import choose, flatten, parallel_map_tqdm, run_ok


class TestChoose:
    def test_no_candidates(self):
        with pytest.raises(ValueError):
            choose([], iter([]))

    def test_cannot_choose(self):
        with pytest.raises(
            ValueError, match=re.escape("Cannot choose between: [10, 20].")
        ):
            choose([10, 20], iter([lambda x: x > 0]))

    def test_short_circuit(self):
        def broken_filter(x: int):
            raise Exception("broken filter")

        assert choose([10, 20], iter([lambda x: x > 15, broken_filter])) == 20


class TestRunOK:
    @pytest.mark.trio
    async def test_success(self):
        assert await run_ok("true")

    @pytest.mark.trio
    async def test_failure(self):
        assert not await run_ok("false")

    @pytest.mark.trio
    async def test_executable_not_found(self):
        assert not await run_ok("nonexistent")


@pytest.mark.trio
async def test_parallel_map_tqdm():
    async def alen(item: str) -> int:
        return len(item)

    results = set(await parallel_map_tqdm(alen, ["one", "two", "three", "four"]))
    assert results == {3, 4, 5}


async def test_flatten():
    assert flatten([[1, 2], [3, 4]]) == [1, 2, 3, 4]
