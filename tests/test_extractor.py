import pytest

from mybox.driver import LocalDriver
from mybox.extractor import Tar, get_extractor


@pytest.mark.trio
async def test_extractor_redirect():
    extractor = await get_extractor(
        "https://telegram.org/dl/desktop/linux", driver=LocalDriver()
    )
    assert isinstance(extractor, Tar)
    assert extractor.extra == ["-J"]


@pytest.mark.trio
async def test_extractor_unknown():
    with pytest.raises(
        ValueError, match="Unknown archive format: https://example.com/"
    ):
        await get_extractor("https://example.com", driver=LocalDriver())
