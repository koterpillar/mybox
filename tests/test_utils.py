import pytest

from mybox.utils import flatten, intercalate, parallel_map_tqdm, run_ok


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


def test_flatten():
    assert flatten([[1, 2], [3, 4]]) == [1, 2, 3, 4]


def test_intercalate():
    assert list(intercalate(0, (x for x in []))) == []
    assert list(intercalate(0, ((x, x + 10) for x in range(1, 4)))) == [
        1,
        11,
        0,
        2,
        12,
        0,
        3,
        13,
    ]
