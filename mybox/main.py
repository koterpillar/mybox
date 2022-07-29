import argparse

from .package import load_packages
from .utils import flatten, parallel_map_tqdm


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Install the environment")
    result.add_argument("component", nargs="*", help="The component to install")
    result.add_argument(
        "--sequential", action="store_true", help="Install packages one at a time"
    )
    return result


def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = flatten(map(load_packages, components))
    map_fn = map if args.sequential else parallel_map_tqdm
    results = map_fn(lambda p: p.ensure(), packages)
    total_installed = sum(1 if installed else 0 for installed in results)
    print(f"{total_installed} packages installed or updated.")
