import argparse
from typing import Optional

import trio
from typed_argparse import TypedArgs

from .driver import LocalDriver
from .package import Package, load_packages
from .state import DB, DB_PATH
from .utils import flatten, parallel_map_tqdm


class Args(TypedArgs):
    component: list[str]


def parse_args(args: Optional[list[str]] = None) -> Args:
    result = argparse.ArgumentParser(description="Install the environment")
    result.add_argument("component", nargs="*", help="The component to install")
    return Args.from_argparse(result.parse_args(args))


async def main():
    db = DB(DB_PATH)
    driver = LocalDriver()
    args = parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = flatten(
        load_packages(component, db=db, driver=driver) for component in components
    )

    async def process(package: Package) -> bool:
        return await package.ensure()

    async def process_and_record(package: Package) -> Optional[str]:
        if await process(package):
            return package.name
        return None

    results = await parallel_map_tqdm(process_and_record, packages)
    installed = list(filter(None, results))
    if installed:
        print(f"{len(installed)} packages installed or updated: {', '.join(installed)}")
    else:
        print("Everything up to date.")


def sync_main():
    trio.run(main)
