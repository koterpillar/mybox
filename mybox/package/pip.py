import json

from .pip_base import PipBasePackage


class PipPackage(PipBasePackage):
    def __init__(self, *, pip: str, **kwargs) -> None:
        self.package = pip.lower()
        super().__init__(**kwargs)

    async def get_all_versions(self) -> dict[str, str]:
        packages_json = await self.driver.run_output(
            *self.cmd("list", "--format", "json"), silent=True
        )

        # https://github.com/pypa/pip/issues/11282
        packages_json = packages_json.split("\n\n[notice]", maxsplit=1)[0]

        packages = json.loads(packages_json)

        return {package["name"].lower(): package["version"] for package in packages}

    def cmd(self, cmd: str, /, *args: str) -> list[str]:
        return ["python3", "-m", "pip", cmd, "--user", *args]
