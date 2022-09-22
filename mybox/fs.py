import os
from pathlib import Path
from typing import Literal, Optional

from .utils import run, run_ok


def find_executable(*executables: str) -> str:
    for candidate in executables:
        if run_ok("which", candidate):
            return candidate
    raise Exception(f"None of {','.join(executables)} found in PATH.")


def is_executable(path: Path) -> bool:
    return run_ok("test", "-x", str(path))


def home() -> Path:
    try:
        return Path(os.environ["MYBOX_HOME"])
    except KeyError:
        pass

    return Path.home()


def local() -> Path:
    return home() / ".local"


def transplant_path(dir_from: Path, dir_to: Path, path: Path) -> Path:
    return dir_to.joinpath(path.relative_to(dir_from))


def makedirs(path: Path, sudo: bool = False) -> None:
    run("mkdir", "-p", str(path), sudo=sudo)


def rm(path: Path, sudo: bool = False) -> None:
    run("rm", "-r", "-f", str(path), sudo=sudo)


def make_executable(path: Path, sudo: bool = False) -> None:
    run("chmod", "+x", str(path), sudo=sudo)


LinkMethod = Literal["binary_wrapper"]


def link(
    source: Path,
    target: Path,
    *,
    sudo: bool = False,
    method: Optional[LinkMethod] = None,
) -> None:
    makedirs(target.parent, sudo=sudo)
    rm(target, sudo=sudo)
    if method == "binary_wrapper":
        if sudo:
            raise NotImplementedError()
        else:
            with open(target, "w") as wrapper_file:
                print(f'#!/bin/sh\nexec "{source}" "$@"', file=wrapper_file)
        make_executable(target, sudo=sudo)
    else:
        run("ln", "-s", "-f", str(source), str(target), sudo=sudo)
