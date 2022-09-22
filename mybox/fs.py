import os
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Any, Literal, Optional

from .utils import run, run_ok, run_output

LinkMethod = Literal["binary_wrapper"]


class FS(metaclass=ABCMeta):
    @abstractmethod
    def run(
        self,
        *args: str,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        stdout: Optional[Any] = None,
    ) -> None:
        pass

    @abstractmethod
    def run_ok(self, *args: str) -> bool:
        pass

    @abstractmethod
    def run_output(self, *args: str, stderr: Optional[Any] = None) -> str:
        pass

    @abstractmethod
    def with_root(self, /, root: bool) -> "FS":
        pass

    def find_executable(self, *executables: str) -> str:
        for candidate in executables:
            if self.run_ok("which", candidate):
                return candidate
        raise Exception(f"None of {','.join(executables)} found in PATH.")

    def is_executable(self, path: Path) -> bool:
        return self.run_ok("test", "-x", str(path))

    def home(self) -> Path:
        # FIXME: root should not use MYBOX_HOME
        try:
            return Path(os.environ["MYBOX_HOME"])
        except KeyError:
            pass

        return Path.home()

    def local(self) -> Path:
        return self.home() / ".local"

    def makedirs(self, path: Path) -> None:
        self.run("mkdir", "-p", str(path))

    def rm(self, path: Path) -> None:
        self.run("rm", "-r", "-f", str(path))

    def make_executable(self, path: Path) -> None:
        self.run("chmod", "+x", str(path))

    def link(
        self,
        source: Path,
        target: Path,
        *,
        method: Optional[LinkMethod] = None,
    ) -> None:
        self.makedirs(target.parent)
        self.rm(target)
        if method == "binary_wrapper":
            # FIXME: should use run
            with open(target, "w") as wrapper_file:
                print(f'#!/bin/sh\nexec "{source}" "$@"', file=wrapper_file)
            self.make_executable(target)
        else:
            self.run("ln", "-s", "-f", str(source), str(target))


class LocalFS(FS):
    def __init__(self, root: bool = False) -> None:
        self.root = root
        super().__init__()

    def run(
        self,
        *args: str,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        stdout: Optional[Any] = None,
    ) -> None:
        run(*args, input=input, stdout=stdout, sudo=self.root)

    def run_ok(self, *args: str) -> bool:
        return run_ok(*args, sudo=self.root)

    def run_output(self, *args: str, stderr: Optional[Any] = None) -> str:
        return run_output(*args, stderr=stderr, sudo=self.root)

    def with_root(self, /, root: bool) -> "LocalFS":
        return LocalFS(root=root)


def transplant_path(dir_from: Path, dir_to: Path, path: Path) -> Path:
    return dir_to.joinpath(path.relative_to(dir_from))
