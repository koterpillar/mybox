from typing import Optional

from .base import PackageCacheInstaller, PackageVersionInfo


class Flatpak(PackageCacheInstaller[PackageVersionInfo]):
    async def install(self, package: str) -> None:
        await self.driver.run("flatpak", "install", "-y", package)
        await super().install(package)

    # can't install an old version of a package in tests
    async def upgrade(self, package: str) -> None:  # pragma: no cover
        await self.driver.run("flatpak", "upgrade", "-y", package)
        await super().upgrade(package)

    @staticmethod
    def parse_versions(output: str) -> dict[str, str]:
        versions = {}
        for line in output.splitlines():
            name, origin, commit = line.split()
            versions[name] = f"{origin}:{commit}"
        return versions

    async def get_installed(self) -> dict[str, str]:
        result = await self.driver.run_output(
            "flatpak", "list", "--app", "--columns=application,origin,active"
        )
        return self.parse_versions(result)

    async def get_latest(self) -> dict[str, str]:
        result = await self.driver.run_output(
            "flatpak", "remote-ls", "--app", "--columns=application,origin,commit"
        )
        return self.parse_versions(result)

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, PackageVersionInfo]:
        if package is not None:
            # Getting all package versions is complete, no package-specific
            # queries needed
            return dict()

        latest = await self.get_latest()
        installed = await self.get_installed()

        return {
            name: PackageVersionInfo(
                installed=installed.get(name), latest=latest_version
            )
            for name, latest_version in latest.items()
        }
