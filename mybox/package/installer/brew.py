import json
from typing import Optional

from ...driver import RunResultOutput
from ...utils import async_cached, async_cached_lock
from .base import PackageCacheInstaller, PackageVersionInfo


class Brew(PackageCacheInstaller):
    async def tap(self, repo: str) -> None:
        await self.driver.run(await self.brew(), "tap", repo)

    async def tapped(self) -> set[str]:
        output = await self.driver.run_output(await self.brew(), "tap")
        return set(output.strip().splitlines())

    async def install(self, package: str) -> None:
        await self.driver.run(await self.brew(), "install", package)
        await super().install(package)

    async def upgrade(self, package: str) -> None:  # pragma: no cover
        await self.driver.run(await self.brew(), "upgrade", package)
        await super().upgrade(package)

    @async_cached
    async def brew(self) -> str:
        # https://docs.brew.sh/FAQ#why-is-the-default-installation-prefix-opthomebrew-on-apple-silicon
        return await self.driver.find_executable("brew", "/opt/homebrew/bin/brew")

    @async_cached_lock
    async def brew_update(self) -> None:
        await self.driver.run(await self.brew(), "update")

    async def brew_info(self, *args: str) -> RunResultOutput:
        return await self.driver.run_output_(
            await self.brew(), "info", "--json=v2", *args
        )

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, PackageVersionInfo]:
        await self.brew_update()

        brew_result: RunResultOutput
        if not package:
            brew_result = await self.brew_info("--installed")
        else:
            brew_result = await self.brew_info(package)

        results: dict[str, PackageVersionInfo] = {}

        if not brew_result.ok:
            return results
        info = json.loads(brew_result.output)

        for cask in info["casks"]:
            if cask["tap"] == "homebrew/cask":
                # Do not require prefixes for casks from homebrew/cask
                name = cask["token"]
            else:
                name = f"{cask['tap']}/{cask['token']}"
            results[name] = PackageVersionInfo(
                installed=cask["installed"], latest=cask["version"]
            )

        for formula in info["formulae"]:
            name = formula["name"]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            latest = self.formula_version(formula)

            if installed and name in results:
                # Cask with the same name exists. This should not happen.
                raise ValueError(
                    f"Found cask and formula with the same name: {name}."
                )  # pragma: no cover
            results[name] = PackageVersionInfo(installed=installed, latest=latest)

        return results

    @staticmethod
    def formula_version(info: dict) -> str:
        version = info["versions"]["stable"]
        revision = info.get("revision")
        if revision:
            version += f"_{revision}"
        return version
