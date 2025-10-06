from mybox.utils import matches_if_specified


def test_matches_if_specified():
    assert matches_if_specified(None, "anything")
    assert not matches_if_specified([], "anything")
    assert matches_if_specified(["alpha", "beta"], "alpha")
    assert not matches_if_specified(["alpha", "beta"], "gamma")
