from pathlib import Path
from typing import AsyncIterator

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
    yield driver
    await driver.stop()
