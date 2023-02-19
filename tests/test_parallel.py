import pytest

from mybox.parallel import PartialResults, parallel_map_tqdm


class TestParallelMapTqdm:
    # Python exceptions don't compare equal (Exception("x") != Exception("x")),
    # so we throw the same exception instance every time.
    ALEN_ERROR = ValueError("too long")

    @classmethod
    async def alen(cls, item: str) -> int:
        length = len(item)
        if length > 5:
            raise cls.ALEN_ERROR
        return length

    @pytest.mark.trio
    async def test_success(self):
        results = await parallel_map_tqdm(self.alen, ["one", "two", "three", "four"])
        assert results == [3, 3, 5, 4]

    @pytest.mark.trio
    async def test_exceptions(self):
        with pytest.raises(PartialResults) as excinfo:
            await parallel_map_tqdm(
                self.alen, ["one", "two", "twelve", "three", "four", "eleven"]
            )

        assert excinfo.value.results == [
            {"tag": "success", "result": 3},
            {"tag": "success", "result": 3},
            {"tag": "exception", "exception": self.ALEN_ERROR},
            {"tag": "success", "result": 5},
            {"tag": "success", "result": 4},
            {"tag": "exception", "exception": self.ALEN_ERROR},
        ]
