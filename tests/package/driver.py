import shlex
import shutil
import tempfile
from os import getpid
from pathlib import Path
from typing import Iterable

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
            return shlex.quote(str(arg))

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


DOCKER_DRIVER_CONTAINER_NUMBER: int = 0


class DockerDriver(TestDriver, SubprocessDriver):
    def __init__(
        self, *, container: str, user: str, docker_sudo: bool = False, **kwargs
    ) -> None:
        super().__init__(**kwargs)
        self.container = container
        self.user = user
        self.docker_sudo = docker_sudo

    def deconstruct(self) -> dict:
        return super().deconstruct() | {
            "container": self.container,
            "user": self.user,
            "docker_sudo": self.docker_sudo,
        }

    @property
    def docker(self) -> list[str]:
        return ["sudo", "docker"] if self.docker_sudo else ["docker"]

    async def stop(self) -> None:
        await run(*self.docker, "rm", "--force", self.container)

    def prepare_command(self, args: Iterable[RunArg]) -> list[RunArg]:
        return super().prepare_command(
            [
                *self.docker,
                "exec",
                *(["--user", "root"] if self.root else []),
                "--interactive",
                self.container,
                *args,
            ]
        )

    container_number = 0

    @classmethod
    async def create(
        cls,
        *,
        image: str,
        user: str = "regular_user",
        docker_sudo: bool = False,
    ) -> "DockerDriver":
        docker = ["sudo", "docker"] if docker_sudo else ["docker"]

        package_root = Path(__file__).parent.parent.parent.absolute()

        bootstrap = package_root / "bootstrap"
        assert bootstrap.is_file()

        target_image = f"mybox-test-{image}"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)
            shutil.copy(bootstrap, tmppath / "bootstrap")
            with open(tmppath / "Dockerfile", "w") as dockerfile:
                dockerfile.write(
                    f"""
                    FROM {image}
                    RUN useradd --create-home --password '' {user}
                    COPY bootstrap /bootstrap
                    RUN /bootstrap --development
                    ENV PATH /home/{user}/.local/bin:$PATH
                    USER {user}
                    # populate dnf cache so each test doesn't have to do it
                    RUN command -v dnf >/dev/null && dnf check-update || true
                """
                )
            await run(*docker, "build", "--tag", target_image, tmppath)

        cls.container_number += 1

        container = await run_output(
            *docker,
            "run",
            "--rm",
            "--detach",
            "--volume",
            f"{package_root}:{package_root}",
            "--name",
            f"mybox-test-{getpid()}-{cls.container_number}",
            target_image,
            "sleep",
            "86400000",
        )
        return cls(container=container, user=user, docker_sudo=docker_sudo)
