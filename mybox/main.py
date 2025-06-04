import sys
from pathlib import Path
from typing import Optional, cast

import trio
import typed_argparse as tap

from .driver import Driver, LocalDriver, SSHDriver
from .manager import Manager
from .state import DB, DB_PATH


class Args(tap.TypedArgs):
    ssh: Optional[str] = tap.arg(help="Install on a remote host via SSH")


def parse_args() -> Args:
    return cast(
        Args, tap.Parser(Args, description="Install the environment").parse_args()
    )


async def main() -> int:
    args = parse_args()

    db = DB(DB_PATH)

    driver: Driver
    if args.ssh:
        driver = SSHDriver(host=args.ssh)
    else:
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
