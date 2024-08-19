import json
from dataclasses import dataclass
from typing import Optional

from ...driver import RunResultOutput
from ...utils import async_cached, async_cached_lock, http_get
from .base import PackageCacheInstaller, PackageVersionInfo


@dataclass
class BrewPackageVersionInfo:
    """
    Brew installer prefers casks to formulae with the same name. Brew itself
    reports both separately, but it requires specifying homebrew/cask/
    everywhere so it's easier for the users to assume a cask if one exists.
    """

    cask: Optional[PackageVersionInfo]
    formula: Optional[PackageVersionInfo]

    @staticmethod
    def formula_version(version: str) -> str:
        """
        To distinguish a cask from a formula of the same name, especially if
        they have the same versions, prepend the formula version with
        "formula_".
        """
        return f"formula_{version}"

    @property
    def installed(self) -> Optional[str]:
        """
        Report installed state of both cask and formula so that any state but
        "cask is installed and formula isn't" results in a different version.
        """
        cask_installed = self.cask.installed if self.cask else None
        formula_installed = (
            self.formula_version(self.formula.installed)
            if self.formula and self.formula.installed
            else None
        )

        if cask_installed and formula_installed:
            return f"{cask_installed} {formula_installed}"
        return cask_installed or formula_installed

    @property
    def latest(self) -> str:
        if self.cask:
            return self.cask.latest
        if self.formula:
            return self.formula_version(self.formula.latest)
        raise ValueError("No cask or formula info.")  # pragma: no cover


class Brew(PackageCacheInstaller[BrewPackageVersionInfo]):
    async def tap(self, repo: str) -> None:
        await self.driver.run(await self.brew(), "tap", repo)

    async def tapped(self) -> set[str]:
        output = await self.driver.run_output(await self.brew(), "tap")
        return set(output.strip().splitlines())

    async def install(self, package: str) -> None:
        info = await self.package_info(package)
        if info.cask:
            await self.driver.run(await self.brew(), "install", "--cask", package)
        else:
            await self.driver.run(await self.brew(), "install", package)
        await super().install(package)

    async def upgrade(self, package: str) -> None:
        info = await self.package_info(package)
        if not info.installed:
            raise ValueError(
                "Expected package to be installed when upgrading."
            )  # pragma: no cover
        if info.cask:
            if info.formula and info.formula.installed:
                # We want a cask, not a formula, uninstall the formula
                await self.driver.run(await self.brew(), "uninstall", package)
            if info.cask.installed:
                # Both formula and cask were installed; formula was uninstalled,
                # upgrade the cask if needed
                if info.cask.latest != info.cask.installed:
                    # Can't install an old version of a package in tests
                    await self.driver.run(
                        await self.brew(), "upgrade", "--cask", package
                    )  # pragma: no cover
            else:
                # Formula was installed (thus upgrading), but cask wasn't
                await self.driver.run(await self.brew(), "install", "--cask", package)
        else:
            # Can't install an old version of a package in tests
            await self.driver.run(
                await self.brew(), "upgrade", package
            )  # pragma: no cover
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

    async def brew_info_cask(self, cask: str) -> RunResultOutput:
        return await self.brew_info(f"homebrew/cask/{cask}")

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, BrewPackageVersionInfo]:
        await self.brew_update()

        brew_result: RunResultOutput
        if not package:
            brew_result = await self.brew_info("--installed")
        elif "/" not in package:
            # When a bare package name is given (without homebrew/, etc. prefix),
            # try to look up the cask first.
            brew_result = await self.brew_info_cask(package)
            if not brew_result.ok:
                brew_result = await self.brew_info(package)
        else:
            brew_result = await self.brew_info(package)

        results: dict[str, BrewPackageVersionInfo] = {}

        if not brew_result.ok:
            return results
        info = json.loads(brew_result.output)

        for cask in info["casks"]:
            if cask["tap"] == "homebrew/cask":
                # Do not require prefixes for casks from homebrew/cask
                name = cask["token"]
            else:
                name = f"{cask['tap']}/{cask['token']}"
            results[name] = BrewPackageVersionInfo(
                cask=PackageVersionInfo(
                    installed=cask["installed"], latest=cask["version"]
                ),
                formula=None,
            )

        for formula in info["formulae"]:
            name = formula["name"]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            latest = self.formula_version(formula)
            pvi = PackageVersionInfo(installed=installed, latest=latest)

            if installed and name in results:
                # Cask with the same name exists. If a _formula_ is installed
                # instead, record the formula version as installed too, so the
                # package will be updated to cask.
                results[name].formula = pvi
            else:
                results[name] = BrewPackageVersionInfo(cask=None, formula=pvi)

        # If queried for installed packages, check for any casks with the same
        # names as installed formulae
        if not package:
            cask_response = await http_get("https://formulae.brew.sh/api/cask.json")

            all_casks = json.loads(cask_response)
            for cask in all_casks:
                name = cask["token"]
                if name not in results:
                    continue  # No formula with this name installed, can ignore
                if results[name].cask:
                    continue  # Already know about this cask
                # Record this cask as available but not installed (it would have
                # been already found otherwise)
                results[name].cask = PackageVersionInfo(
                    installed=None, latest=cask["version"]
                )

        return results

    @staticmethod
    def formula_version(info: dict) -> str:
        version = info["versions"]["stable"]
        revision = info.get("revision")
        if revision:
            version += f"_{revision}"
        return version
