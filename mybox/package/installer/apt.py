from typing import Optional

from .base import Installer


class Apt(Installer):
    async def install(self, package: str) -> None:
        await self.driver.with_root(True).run(
            "env", "DEBIAN_FRONTEND=noninteractive", "apt", "install", "--yes", package
        )

    async def upgrade(self, package: str) -> None:
        await self.install(package)

    async def latest_version(self, package: str) -> str:
        result = await self.driver.run_output_(
            "apt-cache",
            "show",
            "--quiet",
            "--no-all-versions",
            package,
        )
        if not result.ok:
            raise ValueError(f"Cannot determine version for: {package}.")
        for line in result.output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot parse apt output: {result.output}")  # pragma: no cover

    async def installed_version(self, package: str) -> Optional[str]:
        return (
            await self.driver.run_output_(
                "dpkg-query", "--showformat", "${Version}", "--show", package
            )
        ).output
