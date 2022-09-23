from abc import ABCMeta, abstractmethod
from functools import cached_property
from pathlib import Path
from typing import Any, Callable, Iterable, Literal, Optional

from .utils import TERMINAL_LOCK, T, run, run_ok, run_output

LinkMethod = Literal["binary_wrapper"]


class OS(metaclass=ABCMeta):
    def __init__(self, driver: "Driver") -> None:
        self.driver = driver

    @abstractmethod
    def switch_(self, *, linux: Callable[["Linux"], T], macos: T) -> T:
        pass

    def switch(self, *, linux: T, macos: T) -> T:
        return self.switch_(linux=lambda _: linux, macos=macos)


class Linux(OS):
    def switch_(self, *, linux: Callable[["Linux"], T], macos: T) -> T:
        return linux(self)

    RELEASE_FILE = "/etc/os-release"

    @cached_property
    def distribution(self) -> str:
        for line in self.driver.read_file(Path(self.RELEASE_FILE)).splitlines():
            k, v = line.split("=", 1)
            if k == "ID":
                return v

        raise ValueError(f"Cannot find distribution ID in {self.RELEASE_FILE}.")


class MacOS(OS):
    def switch_(self, *, linux: Callable[["Linux"], T], macos: T) -> T:
        return macos


class Driver(metaclass=ABCMeta):
    def __init__(self, root: bool = False) -> None:
        self.root = root
        super().__init__()

    def with_root(self, /, root: bool) -> "Driver":
        return type(self)(root=root)

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

    def find_executable(self, *executables: str) -> str:
        for candidate in executables:
            if self.run_ok("which", candidate):
                return candidate
        raise Exception(f"None of {','.join(executables)} found in PATH.")

    def is_file(self, path: Path) -> bool:
        return self.run_ok("test", "-f", str(path))

    def is_executable(self, path: Path) -> bool:
        return self.run_ok("test", "-x", str(path))

    def is_dir(self, path: Path) -> bool:
        return self.run_ok("test", "-d", str(path))

    def home(self) -> Path:
        path = "~root" if self.root else "~"
        return Path(self.with_root(False).run_output("sh", "-c", f"eval echo {path}"))

    def local(self) -> Path:
        return self.home() / ".local"

    def makedirs(self, path: Path) -> None:
        self.run("mkdir", "-p", str(path))

    def rm(self, path: Path) -> None:
        self.run("rm", "-r", "-f", str(path))

    def make_executable(self, path: Path) -> None:
        self.run("chmod", "+x", str(path))

    def read_file(self, path: Path) -> str:
        return self.run_output("cat", str(path))

    def write_file(self, path: Path, content: str) -> None:
        self.run("cp", "/dev/stdin", str(path), input=content.encode())

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
            self.write_file(target, f'#!/bin/sh\nexec "{source}" "$@"')
            self.make_executable(target)
        else:
            self.run("ln", "-s", "-f", str(source), str(target))

    @cached_property
    def os(self) -> OS:
        os_type = self.run_output("uname")
        if os_type == "Linux":
            return Linux(self)
        elif os_type == "Darwin":
            return MacOS(self)
        else:
            raise ValueError(f"Unsupported OS type {os_type}.")


class LocalDriver(Driver):
    def run_args(self, args: Iterable[str]) -> list[str]:
        if self.root:
            # If the sudo prompt is needed, avoid drawing a progress bar over it
            # with first prompting for a no-op command.
            with TERMINAL_LOCK:
                run("sudo", "true")
            return ["sudo", *args]
        else:
            return list(args)

    def run(
        self,
        *args: str,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        stdout: Optional[Any] = None,
    ) -> None:
        run(*self.run_args(args), input=input, stdout=stdout)

    def run_ok(self, *args: str) -> bool:
        return run_ok(*self.run_args(args))

    def run_output(self, *args: str, stderr: Optional[Any] = None) -> str:
        return run_output(*self.run_args(args), stderr=stderr)


def transplant_path(dir_from: Path, dir_to: Path, path: Path) -> Path:
    return dir_to.joinpath(path.relative_to(dir_from))
