from pathlib import Path

from .base import PackageArgs, PackageTestBase


class TestExpress(PackageTestBase):
    async def constructor_args(self) -> PackageArgs:
        return {"npm": "express-generator", "binary": "express"}

    prerequisites = [
        *PackageTestBase.NODE,
        {"name": "npm", "os": "linux"},
    ]

    async def check_installed_command(self):
        return ["express", "--help"]

    check_installed_output = "engine support"

    async def ignored_paths(self) -> set[Path]:
        return await super().ignored_paths() | {await self.check_driver.home() / ".npm"}
