import pytest

from mybox.config import MatchConfig

from .base import DummyDriver


@pytest.mark.trio
async def test_match_config_no_host():
    """Matches any hostname with no host pattern"""
    driver = DummyDriver(hostname="mango-one")
    config = MatchConfig(driver=driver)
    assert await config.applicable()


@pytest.mark.trio
async def test_match_config_matching_host():
    """Test when hostname matches the pattern"""
    driver = DummyDriver(hostname="mango-one")
    config = MatchConfig(host=["mango-*"], driver=driver)
    assert await config.applicable()


@pytest.mark.trio
async def test_match_config_non_matching_host():
    """Test when hostname doesn't match the pattern"""
    driver = DummyDriver(hostname="raspberry-one")
    config = MatchConfig(host=["mango-*"], driver=driver)
    assert not await config.applicable()


@pytest.mark.trio
async def test_match_config_multiple_patterns():
    """Test when multiple host patterns are specified"""
    driver = DummyDriver(hostname="raspberry-one")
    config = MatchConfig(host=["mango-*", "raspberry-*"], driver=driver)
    assert await config.applicable()


@pytest.mark.trio
async def test_match_config_exact_match():
    """Test exact hostname match"""
    driver = DummyDriver(hostname="mango-one")
    config = MatchConfig(host=["mango-one"], driver=driver)
    assert await config.applicable()


@pytest.mark.trio
async def test_match_config_components():
    """Test getting components from matching configs"""
    driver = DummyDriver(hostname="raspberry-one")

    configs = [
        MatchConfig(host=["raspberry-*"], component=["comp1"], driver=driver),
        MatchConfig(host=["mango-*"], component=["comp2"], driver=driver),
    ]

    components = await MatchConfig.components(configs)
    assert "comp1" in components
    assert "comp2" not in components
