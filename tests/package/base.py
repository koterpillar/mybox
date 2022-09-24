import os
import shutil
import tempfile
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Any, Iterable, Optional

import pytest

from mybox.driver import Driver, LocalDriver, SubprocessDriver
from mybox.package import parse_package
from mybox.state import DB
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


class PackageTestBase(metaclass=ABCMeta):
    driver: Driver

    @property
    @abstractmethod
    def constructor_args(self) -> dict[str, Any]:
        pass

    @property
    @abstractmethod
    def check_installed_command(self) -> Iterable[str]:
        pass

    check_installed_output: Optional[str] = None

    @pytest.fixture(autouse=True)
    def ensure_local_bin_environment(
        self, monkeypatch: pytest.MonkeyPatch, tmp_path: Path
    ) -> None:
        docker_image = os.environ.get("DOCKER_IMAGE")
        if docker_image:
            self.driver = DockerDriver.create(image=docker_image)
        else:
            local_bin = tmp_path / ".local" / "bin"
            monkeypatch.setenv("PATH", str(local_bin.absolute()), prepend=":")
            self.driver = OverrideHomeDriver(override_home=tmp_path)

    @property
    def test_driver(self) -> Driver:
        return self.driver

    def check_installed(self):
        if self.check_installed_output is None:
            self.test_driver.run_ok(*self.check_installed_command)
        else:
            output = self.test_driver.run_output(*self.check_installed_command)
            assert self.check_installed_output in output

    def test_installs(self):
        db = DB(":memory:")
        package = parse_package(self.constructor_args, db=db, driver=self.driver)
        assert package.applicable
        package.install()
        self.check_installed()
