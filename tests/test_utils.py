import re

import pytest

from mybox.utils import choose, run_ok


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
    def test_success(self):
        assert run_ok("true")

    def test_failure(self):
        assert not run_ok("false")

    def test_executable_not_found(self):
        assert not run_ok("nonexistent")
