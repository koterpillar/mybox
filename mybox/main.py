import argparse
from pathlib import Path
from typing import Optional

import trio
from typed_argparse import TypedArgs

from .driver import LocalDriver
from .manager import Manager
from .state import DB, DB_PATH


class Args(TypedArgs):
    component: list[str]


def parse_args(args: Optional[list[str]] = None) -> Args:
    result = argparse.ArgumentParser(description="Install the environment")
    result.add_argument("component", nargs="*", help="The component to install")
    return Args.from_argparse(result.parse_args(args))


async def main():
    args = parse_args()

    db = DB(DB_PATH)
    driver = LocalDriver()
    manager = Manager(db=db, driver=driver, component_path=Path("packages"))

    components: frozenset[str] = frozenset(args.component) | {"base"}

    installed = await manager.install(components)

    if installed:
        print(
            f"{len(installed)} packages installed or updated: {', '.join(p.name for p in installed)}"
        )
    else:
        print("Everything up to date.")


def sync_main():
    trio.run(main)
