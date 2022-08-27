import argparse

from .package import load_packages
from .state.base import DB, DB_PATH
from .utils import flatten, parallel_map_tqdm


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Install the environment")
    result.add_argument("component", nargs="*", help="The component to install")
    result.add_argument(
        "--sequential", action="store_true", help="Install packages one at a time"
    )
    return result


def main():
    db = DB(DB_PATH)
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = flatten(load_packages(db, component) for component in components)
    map_fn = map if args.sequential else parallel_map_tqdm
    results = map_fn(lambda p: p.name if p.ensure() else None, packages)
    installed = list(filter(None, results))
    if installed:
        print(
            f"{len(installed)} packages installed or updated: {', '   .join(installed)}"
        )
    else:
        print("Everything up to date.")
