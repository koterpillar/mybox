from typing import Optional

from .base import PackageCacheInstaller, PackageVersionInfo


class DNF(PackageCacheInstaller[PackageVersionInfo]):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run("dnf", "install", "-y", package)
        await super().install(package)

    # can't install an old version of a package in tests
    async def upgrade(self, package: str) -> None:  # pragma: no cover
        await self.driver.with_root(True).run("dnf", "upgrade", "-y", package)
        await super().upgrade(package)

    @staticmethod
    def parse_versions(output: str, package: Optional[str]) -> dict[str, str]:
        versions = {}
        for line in output.splitlines():
            name, version = line.split()
            versions[name] = version

        if package is not None:
            if package in versions:
                return {package: versions[package]}
            # If querying for a specific package, the name might be different
            # because of virtual packages and --whatprovides option.
            # If there's no package with exact name, require a single match.
            if len(versions) > 1:
                raise ValueError(
                    f"Multiple versions for {package}: {versions}."
                )  # pragma: no cover
            if len(versions) == 0:
                raise ValueError(f"No versions for {package}.")
            (version,) = versions.values()  # pylint:disable=unbalanced-dict-unpacking
            return {package: version}

        return versions

    async def rpm_query(self, package: Optional[str]) -> dict[str, str]:
        """
        Query RPM for the installed package versions.

        @param package: If specified, only query this package; otherwise,
        return all packages' versions.
        """
        args = [
            "rpm",
            "--query",
            "--queryformat",
            "%{NAME} %{VERSION}\n",
        ]

        if package:
            args += ["--whatprovides", package]
        else:
            args += ["--all"]

        result = await self.driver.run_output_(*args)
        if not result.ok:
            return {}
        return self.parse_versions(result.output, package)

    async def dnf_repoquery(self, package: Optional[str]) -> dict[str, str]:
        """
        Query DNF for the latest package versions.

        @param package: If specified, only query this package; otherwise,
        return all packages' versions.
        """

        arch = await self.driver.architecture()

        args = [
            "dnf",
            "--quiet",
            "repoquery",
            "--queryformat",
            "%{name} %{version}",
            "--latest-limit",
            "1",
            "--arch",
            f"{arch},noarch",
        ]

        if package:
            args += ["--whatprovides", package]

        output = await self.driver.run_output(*args)
        return self.parse_versions(output, package)

    async def get_package_info(
        self, package: Optional[str]
    ) -> dict[str, PackageVersionInfo]:
        latest = await self.dnf_repoquery(package)
        installed = await self.rpm_query(package)

        return {
            name: PackageVersionInfo(
                installed=installed.get(name), latest=latest_version
            )
            for name, latest_version in latest.items()
        }
