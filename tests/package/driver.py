import shutil
import tempfile
from collections.abc import Sequence
from os import getpid
from pathlib import Path

from mybox.driver import LocalDriver
from mybox.utils import RunArg, run, run_output

from ..base import PACKAGE_ROOT


class TestDriver(LocalDriver):
    __test__ = False

    async def stop(self) -> None:
        pass

    @classmethod
    async def create(cls) -> "TestDriver":
        driver = TestDriver()
        await driver.run(PACKAGE_ROOT / "bootstrap", "--development")
        return driver


DOCKER_USER = "regular_user"

DOCKER_IMAGE_PREFIX = "mybox-test-"

DOCKER_DRIVER_CONTAINER_NUMBER: int = 0


class DockerDriver(TestDriver):
    def __init__(self, *, container: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.container = container

    async def stop(self) -> None:
        await run("docker", "rm", "--force", self.container)

    def prepare_command(self, args: Sequence[RunArg]) -> list[RunArg]:
        root = False
        if args and args[0] == "sudo":
            root = True
            args = args[1:]

        return super().prepare_command(
            [
                "docker",
                "exec",
                *(["--user", "root"] if root else []),
                "--interactive",
                self.container,
                *args,
            ]
        )

    container_number = 0

    @classmethod
    async def create_docker(cls, *, image: str) -> "DockerDriver":
        target_image = f"{DOCKER_IMAGE_PREFIX}{image}"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            shutil.copy(PACKAGE_ROOT / "bootstrap", tmppath / "bootstrap")
            with open(tmppath / "Dockerfile", "w") as dockerfile:
                dockerfile.write(
                    f"""
                    FROM {image}
                    RUN useradd --create-home --password '' {DOCKER_USER}
                    COPY bootstrap /bootstrap
                    RUN /bootstrap --development
                    ENV PATH=/home/{DOCKER_USER}/.local/bin:$PATH
                    USER {DOCKER_USER}
                    # populate dnf cache so each test doesn't have to do it
                    RUN command -v dnf >/dev/null && dnf check-update || true
                """
                )
            await run("docker", "build", "--tag", target_image, tmppath)

        cls.container_number += 1

        container = await run_output(
            "docker",
            "run",
            "--rm",
            "--detach",
            "--volume",
            f"{PACKAGE_ROOT}:{PACKAGE_ROOT}",
            "--name",
            f"mybox-test-{getpid()}-{cls.container_number}",
            target_image,
            "sleep",
            "86400000",
        )
        return cls(container=container)
