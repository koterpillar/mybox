import re

import pytest

from mybox.filters import Filters, choose


def test_filters_attributes():
    filters = Filters(prefix=["a", "b"], suffix="c", include="2", exclude="3")
    assert choose(["a1c", "a2c", "a23c", "b1d", "a1e"], filters.filters()) == "a2c"


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
