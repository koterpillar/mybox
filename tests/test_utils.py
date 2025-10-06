import pytest

from mybox.utils import (
    flatten,
    matches_if_specified,
    run_ok,
)


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


def test_flatten():
    assert flatten([[1, 2], [3, 4]]) == [1, 2, 3, 4]


def test_matches_if_specified():
    assert matches_if_specified(None, "anything")
    assert not matches_if_specified([], "anything")
    assert matches_if_specified(["alpha", "beta"], "alpha")
    assert not matches_if_specified(["alpha", "beta"], "gamma")
