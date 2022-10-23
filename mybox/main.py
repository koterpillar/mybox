import argparse
from typing import Optional

from typed_argparse import TypedArgs

from .driver import LocalDriver
from .package import Package, load_packages
from .state import DB, DB_PATH
from .utils import flatten, parallel_map_tqdm


class Args(TypedArgs):
    component: list[str]
    sequential: bool


def parse_args(args: Optional[list[str]] = None) -> Args:
    result = argparse.ArgumentParser(description="Install the environment")
    result.add_argument("component", nargs="*", help="The component to install")
    result.add_argument(
        "--sequential", action="store_true", help="Install packages one at a time"
    )
    return Args.from_argparse(result.parse_args(args))


def main():
    db = DB(DB_PATH)
    driver = LocalDriver()
    args = parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = flatten(
        load_packages(component, db=db, driver=driver) for component in components
    )
    map_fn = map if args.sequential else parallel_map_tqdm

    def process(package: Package) -> bool:
        return package.ensure()

    def process_and_record(package: Package) -> Optional[str]:
        if process(package):
            return package.name
        return None

    results = map_fn(process_and_record, packages)
    installed = list(filter(None, results))
    if installed:
        print(f"{len(installed)} packages installed or updated: {', '.join(installed)}")
    else:
        print("Everything up to date.")
