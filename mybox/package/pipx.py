import json

from .pip_base import PipBasePackage


class PipxPackage(PipBasePackage):
    def __init__(self, *, pipx: str, **kwargs) -> None:
        super().__init__(**kwargs)
        self.package = pipx

    async def get_all_versions(self) -> dict[str, str]:
        pipx_list = json.loads(
            await self.driver.run_output(*self.cmd("list", "--json"), silent=True)
        )
        packages = (
            item["metadata"]["main_package"] for item in pipx_list["venvs"].values()
        )
        return {package["package"]: package["package_version"] for package in packages}

    def cmd(self, cmd: str, /, *args: str) -> list[str]:
        return ["python3", "-m", "pipx", cmd, *args]
