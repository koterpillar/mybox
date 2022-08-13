import os
import shutil
from pathlib import Path
from typing import Literal, Optional

from .utils import run


def find_executable(*executables: str) -> str:
    for candidate in executables:
        if shutil.which(candidate):
            return candidate
    raise Exception(f"None of {','.join(executables)} found in PATH.")


def is_executable(path: Path) -> bool:
    return os.access(path, os.X_OK)


LN = find_executable("gln", "ln")


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
    if sudo:
        run("sudo", "mkdir", "-p", str(path))
    else:
        path.mkdir(parents=True, exist_ok=True)


def rm(path: Path, sudo: bool = False) -> None:
    if sudo:
        run("sudo", "rm", "-r", "-f", str(path))
    else:
        path.unlink(missing_ok=True)


def make_executable(path: Path) -> None:
    run("chmod", "+x", str(path))


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
            make_executable(target)
    else:
        if sudo:
            run("sudo", LN, "-s", "-f", "-T", str(source), str(target))
        else:
            target.symlink_to(source)
