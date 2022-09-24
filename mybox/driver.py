import subprocess
from abc import ABCMeta, abstractmethod
from dataclasses import dataclass
from functools import cached_property
from pathlib import Path
from typing import Callable, Iterable, Literal, Optional, cast

from .utils import TERMINAL_LOCK, T

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


@dataclass
class RunResult:
    ok: bool
    output: Optional[str]


class Driver(metaclass=ABCMeta):
    def __init__(self, *, root: bool = False) -> None:
        self.root = root
        super().__init__()

    def deconstruct(self) -> dict:
        return {"root": self.root}

    def with_root(self, /, root: bool) -> "Driver":
        if self.root == root:
            return self
        kwargs = self.deconstruct() | {"root": root}
        return type(self)(**kwargs)

    @abstractmethod
    def run_(
        self,
        *args: str,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        pass

    def run(
        self,
        *args: str,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        silent: bool = False,
    ) -> None:
        self.run_(*args, input=input, silent=silent)

    def run_ok(self, *args: str) -> bool:
        return self.run_(*args, check=False).ok

    def run_output(self, *args: str, silent: bool = False) -> str:
        return cast(str, self.run_(*args, capture_output=True, silent=silent).output)

    def executable_exists(self, executable: str) -> bool:
        return self.run_ok("sh", "-c", f"command -v {executable}")

    def find_executable(self, *executables: str) -> str:
        for candidate in executables:
            if self.executable_exists(candidate):
                return candidate
        raise Exception(f"None of {', '.join(executables)} found in PATH.")

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
        driver = self.with_root(False)
        os_type = driver.run_output("uname")
        if os_type == "Linux":
            return Linux(driver)
        elif os_type == "Darwin":
            return MacOS(driver)
        else:
            raise ValueError(f"Unsupported OS type {os_type}.")


class SubprocessDriver(Driver, metaclass=ABCMeta):
    def prepare_command(self, args: Iterable[str]) -> list[str]:
        return list(args)

    def run_(
        self,
        *args: str,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        command = self.prepare_command(args)

        if capture_output:
            stdout = subprocess.PIPE
        elif not check:
            stdout = subprocess.DEVNULL
        else:
            stdout = None

        if not check or silent:
            stderr = subprocess.DEVNULL
        else:
            stderr = None

        result = subprocess.run(
            command, check=check, input=input, stdout=stdout, stderr=stderr
        )

        ok = result.returncode == 0
        output: Optional[str]
        if ok and capture_output:
            output = result.stdout.decode().strip()
        else:
            output = None
        return RunResult(ok=result.returncode == 0, output=output)


class LocalDriver(SubprocessDriver):
    def prepare_command(self, args: Iterable[str]) -> list[str]:
        if self.root:
            # If the sudo prompt is needed, avoid drawing a progress bar over it
            # with first prompting for a no-op command.
            with TERMINAL_LOCK:
                subprocess.run(["sudo", "true"], check=True)
            return super().prepare_command(["sudo", *args])
        else:
            return super().prepare_command(args)
