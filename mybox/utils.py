import concurrent.futures
import os
import subprocess
import sys
from threading import Lock
from typing import Callable, Iterable, Literal, Optional, TypeVar, Union, cast

import tqdm  # type: ignore

OS = Literal["linux", "darwin"]

CURRENT_OS: OS = cast(OS, sys.platform)


Distribution = str


def get_distro() -> Optional[Distribution]:
    if CURRENT_OS != "linux":
        return None

    release_file = "/etc/os-release"
    with open(release_file) as release:
        for line in release:
            k, v = line.split("=", 1)
            if k == "ID":
                return v

    raise ValueError(f"Cannot find distribution ID in {release_file}.")


CURRENT_DISTRIBUTION = get_distro()

T = TypeVar("T")
U = TypeVar("U")


def with_os(*, linux: T, macos: T) -> T:
    if CURRENT_OS == "linux":
        return linux
    elif CURRENT_OS == "darwin":
        return macos
    else:
        raise ValueError(f"Unexpected OS {CURRENT_OS}.")


Some = Optional[Union[T, list[T]]]


def unsome_(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


def unsome(x: Some[T]) -> list[T]:
    return unsome_(x) or []


TERMINAL_LOCK = Lock()


def run(*args: str, **kwargs) -> subprocess.CompletedProcess:
    if args[0] == "sudo":
        # If the sudo prompt is needed, avoid drawing a progress bar over it
        # with first prompting for a no-op command.
        with TERMINAL_LOCK:
            subprocess.run(["sudo", "true"], check=True)
    return subprocess.run(args, check=True, **kwargs)


def run_ok(*args: str, **kwargs) -> bool:
    return (
        subprocess.run(
            args,
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            **kwargs,
        ).returncode
        == 0
    )


def run_output(*args: str, **kwargs) -> str:
    return (
        subprocess.run(args, stdout=subprocess.PIPE, check=True, **kwargs)
        .stdout.decode()
        .strip()
    )


def home(*path: str) -> str:
    return os.path.join(os.environ["HOME"], *path)


def local(*path: str) -> str:
    return home(".local", *path)


def flatten(items: Iterable[Iterable[T]]) -> list[T]:
    return [item for sublist in items for item in sublist]


def parallel_map_tqdm(items: list[T], action: Callable[[T], U]) -> list[U]:
    with tqdm.tqdm(total=len(items)) as progress:
        with concurrent.futures.ThreadPoolExecutor(20) as executor:

            def action_and_update(item: T) -> U:
                result = action(item)
                with TERMINAL_LOCK:
                    progress.update(1)
                return result

            return list(executor.map(action_and_update, items))


class Filters:
    @staticmethod
    def includes(substring: str) -> Callable[[str], bool]:
        return lambda x: substring in x.lower()

    @staticmethod
    def excludes(substring: str) -> Callable[[str], bool]:
        return lambda x: substring not in x

    @staticmethod
    def startswith(prefix: str) -> Callable[[str], bool]:
        return lambda x: x.startswith(prefix)

    @staticmethod
    def endswith(suffix: str) -> Callable[[str], bool]:
        return lambda x: x.endswith(suffix)
