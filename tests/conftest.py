from collections.abc import AsyncIterator
from pathlib import Path

import pytest

from .base import DOCKER_IMAGE
from .package.driver import DockerDriver, OverrideHomeDriver, TestDriver

pytest.register_assert_rewrite("tests.package.base")


@pytest.fixture(name="make_driver")
async def fixture_driver(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> AsyncIterator[TestDriver]:
    driver: TestDriver

    if DOCKER_IMAGE:
        driver = await DockerDriver.create(image=DOCKER_IMAGE)

    else:
        local_bin = tmp_path / ".local" / "bin"
        monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")

        driver = OverrideHomeDriver(override_home=tmp_path)

        # Required for fc-cache
        await driver.link_to_real_home(".local", "share", "fonts")

    yield driver
    await driver.stop()
