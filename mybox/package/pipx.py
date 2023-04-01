import json

from pydantic import Field, validator

from .pip_base import PipBasePackage

PIPX = ["python3", "-m", "pipx"]


class PipxPackage(PipBasePackage):
    package: str = Field(..., alias="pipx")

    @validator("package")
    def package_to_lower(cls, value: str) -> str:  # pylint: disable=no-self-argument
        return value.lower()

    async def get_all_versions(self) -> dict[str, str]:
        pipx_list = json.loads(
            await self.driver.run_output(*PIPX, "list", "--json", silent=True)
        )
        packages = (
            item["metadata"]["main_package"] for item in pipx_list["venvs"].values()
        )
        return {package["package"]: package["package_version"] for package in packages}

    async def install(self) -> None:
        cmd = "install" if await self.local_version() is None else "upgrade"
        await self.driver.run(*PIPX, cmd, self.package)
        await super().install()
