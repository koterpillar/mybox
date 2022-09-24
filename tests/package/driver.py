import shutil
import tempfile
from pathlib import Path
from typing import Iterable

from mybox.driver import LocalDriver, SubprocessDriver
from mybox.utils import run, run_output


class OverrideHomeDriver(LocalDriver):
    def __init__(self, *, root: bool = False, override_home: Path) -> None:
        super().__init__(root=root)
        self.override_home = override_home

    def deconstruct(self) -> dict:
        return super().deconstruct() | {"override_home": self.override_home}

    def home(self) -> Path:
        if self.root:
            return super().home()
        return self.override_home


class DockerDriver(SubprocessDriver):
    def __init__(
        self,
        *,
        root: bool = False,
        container: str,
        user: str,
        docker_sudo: bool = False,
    ) -> None:
        super().__init__(root=root)
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

    def stop(self) -> None:
        run(*self.docker, "rm", "--force", self.container)

    def prepare_command(self, args: Iterable[str]) -> list[str]:
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

    @classmethod
    def create(
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
                    RUN useradd --create-home {user}
                    COPY bootstrap /bootstrap
                    RUN /bootstrap --development
                    ENV PATH /home/{user}/.local/bin:$PATH
                    USER {user}
                """
                )
            run(*docker, "build", "--tag", target_image, str(tmppath))

        container = run_output(
            *docker,
            "run",
            "--rm",
            "--detach",
            "--volume",
            f"{package_root}:{package_root}",
            target_image,
            "sleep",
            "300",
        )
        return cls(container=container, user=user, docker_sudo=docker_sudo)
