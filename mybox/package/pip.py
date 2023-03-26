import json

from pydantic import Field, validator

from .pip_base import PIP, PipBasePackage


class PipPackage(PipBasePackage):
    package: str = Field(..., alias="pip")

    @validator("package")
    def package_to_lower(cls, value: str) -> str:  # pylint: disable=no-self-argument
        return value.lower()

    async def get_all_versions(self) -> dict[str, str]:
        packages_json = await self.driver.run_output(
            *PIP, "list", "--format", "json", silent=True
        )

        # https://github.com/pypa/pip/issues/11282
        packages_json = packages_json.split("\n\n[notice]", maxsplit=1)[0]

        packages = json.loads(packages_json)

        return {package["name"].lower(): package["version"] for package in packages}

    async def install(self) -> None:
        await self.driver.run(*PIP, "install", "--upgrade", "--user", self.package)
        await super().install()
