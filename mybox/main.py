import argparse

from .package import load_packages
from .utils import flatten, parallel_map_tqdm


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description="Install the environment")
    result.add_argument("component", nargs="*", help="The component to install")
    return result


def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = flatten(map(load_packages, components))
    parallel_map_tqdm(packages, lambda p: p.ensure())
