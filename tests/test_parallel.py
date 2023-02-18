import pytest

from mybox.parallel import parallel_map_tqdm


@pytest.mark.trio
async def test_parallel_map_tqdm():
    async def alen(item: str) -> int:
        return len(item)

    results = set(await parallel_map_tqdm(alen, ["one", "two", "three", "four"]))
    assert results == {3, 4, 5}
