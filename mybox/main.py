import argparse
import sys
from pathlib import Path
from typing import Optional

import trio
from typed_argparse import TypedArgs

from .driver import LocalDriver
from .manager import Manager
from .state import DB, DB_PATH


class Args(TypedArgs):
    pass


def parse_args(args: Optional[list[str]] = None) -> Args:
    result = argparse.ArgumentParser(description="Install the environment")
    return Args.from_argparse(result.parse_args(args))


async def main() -> int:
    parse_args()

    db = DB(DB_PATH)
    driver = LocalDriver()
    manager = Manager(db=db, driver=driver, data_path=Path())

    result = await manager.install()

    if result.installed:
        print(
            f"{len(result.installed)} packages installed or updated: {', '.join(p.name for p in result.installed)}"
        )
    if result.failed:
        for p, exc in result.failed:
            print(f"Failed to install {p.name}: {exc}")

    if not result.installed and not result.failed:
        print("Everything up to date.")

    return len(result.failed)


def sync_main() -> None:
    sys.exit(trio.run(main))
