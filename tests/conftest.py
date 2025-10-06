from collections.abc import AsyncIterator

import pytest

from .base import DOCKER_IMAGE
from .package.driver import DockerDriver, TestDriver

pytest.register_assert_rewrite("tests.package.base")


@pytest.fixture(name="make_driver")
async def fixture_driver() -> AsyncIterator[TestDriver]:
    driver: TestDriver

    if DOCKER_IMAGE:
        driver = await DockerDriver.create_docker(image=DOCKER_IMAGE)

    else:
        driver = await TestDriver.create()

    yield driver
    await driver.stop()
