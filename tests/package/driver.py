import shlex
import shutil
import tempfile
from collections.abc import Iterable
from os import getpid
from pathlib import Path

from mybox.driver import Driver, LocalDriver, RunResult, SubprocessDriver
from mybox.utils import RunArg, run, run_output


class TestDriver(Driver):
    __test__ = False

    def __init__(self, *, enable_root: bool = True, **kwargs) -> None:
        super().__init__(**kwargs)
        self.enable_root = enable_root

    def deconstruct(self) -> dict:
        return super().deconstruct() | {"enable_root": self.enable_root}

    def disable_root(self) -> "Driver":
        kwargs = self.deconstruct() | {"enable_root": False}
        return type(self)(**kwargs)

    def log_command(self, args: Iterable[RunArg]) -> None:
        def show_arg(arg: RunArg) -> str:
            result = str(arg)
            result = shlex.quote(result)
            result = result.replace("\n", "\\n")
            return result

        prompt_symbol = "#" if self.root else "$"
        print(f"->{prompt_symbol}", *map(show_arg, args))

    async def run_(self, *args, **kwargs) -> RunResult:
        if not self.enable_root:
            assert not self.root, "Root operations are disabled."
        self.log_command(args)
        result = await super().run_(*args, **kwargs)
        return result

    async def stop(self) -> None:
        pass


def package_root() -> Path:
    return Path(__file__).parent.parent.parent.absolute()


def bootstrap_script() -> Path:
    bootstrap = package_root() / "bootstrap"
    assert bootstrap.is_file()

    return bootstrap


class OverrideHomeDriver(TestDriver, LocalDriver):
    def __init__(self, *, override_home: Path, **kwargs) -> None:
        super().__init__(**kwargs)
        self.override_home = override_home

    def deconstruct(self) -> dict:
        return super().deconstruct() | {"override_home": self.override_home}

    async def home(self) -> Path:
        if self.root:
            return await super().home()
        return self.override_home

    async def link_to_real_home(self, *path: str) -> None:
        overridden_dir = Path(self.override_home, *path)
        real_dir = Path.home() / Path(*path)

        await self.makedirs(real_dir)
        await self.link(real_dir, overridden_dir)

    @classmethod
    async def create(cls, *, override_home: Path) -> "OverrideHomeDriver":
        driver = OverrideHomeDriver(override_home=override_home)
        await driver.run(bootstrap_script(), "--development")
        return driver


DOCKER_USER = "regular_user"

DOCKER_IMAGE_PREFIX = "mybox-test-"

DOCKER_DRIVER_CONTAINER_NUMBER: int = 0


class DockerDriver(TestDriver, SubprocessDriver):
    def __init__(self, *, container: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.container = container

    def deconstruct(self) -> dict:
        return super().deconstruct() | {"container": self.container}

    async def stop(self) -> None:
        await run("docker", "rm", "--force", self.container)

    def prepare_command(self, args: Iterable[RunArg]) -> list[RunArg]:
        return super().prepare_command(
            [
                "docker",
                "exec",
                *(["--user", "root"] if self.root else []),
                "--interactive",
                self.container,
                *args,
            ]
        )

    container_number = 0

    @classmethod
    async def create(cls, *, image: str) -> "DockerDriver":
        target_image = f"{DOCKER_IMAGE_PREFIX}{image}"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            shutil.copy(bootstrap_script(), tmppath / "bootstrap")
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
            f"{package_root()}:{package_root()}",
            "--name",
            f"mybox-test-{getpid()}-{cls.container_number}",
            target_image,
            "sleep",
            "86400000",
        )
        return cls(container=container)
