import json
import re
from pathlib import Path
from typing import Any, Optional, cast

from pydantic import Field, validator

from ..tracker import Tracker
from .base import Package


class PipxPackage(Package):
    package: str = Field(..., alias="pipx")

    @validator("package")
    def package_to_lower(cls, value: str) -> str:  # pylint: disable=no-self-argument
        return value.lower()

    def derive_name(self) -> str:
        return self.package

    async def get_metadata(self) -> Optional[dict[str, Any]]:
        pipx_list = json.loads(
            await self.driver.run_output("pipx", "list", "--json", silent=True)
        )
        venv = pipx_list["venvs"].get(self.package)
        if venv:
            return venv["metadata"]["main_package"]
        else:
            return None

    async def local_version(self) -> Optional[str]:
        metadata = await self.get_metadata()
        if metadata:
            return metadata["package_version"]
        else:
            return None

    async def get_remote_version(self) -> str:
        check = await self.driver.run_(
            "python3",
            "-m",
            "pip",
            "index",
            "versions",
            self.package,
            check=False,
            silent=True,
            capture_output=True,
        )
        if not check.ok:
            raise Exception(f"Cannot find latest version of package '{self.package}'.")
        output = cast(str, check.output)
        version = re.search(r"\(([^)]+)\)", output)
        if not version:
            raise Exception(f"Cannot parse pip output: {output}")  # pragma: no cover
        return version[1]

    async def install(self, *, tracker: Tracker) -> None:
        cmd = "install" if await self.local_version() is None else "upgrade"
        await self.driver.run("pipx", cmd, self.package)

        tracker.track(await self.driver.local() / "pipx" / "venvs" / self.package)
        metadata = await self.get_metadata()
        if metadata:
            for bin_desc in metadata["app_paths"]:
                bin_path = Path(bin_desc["__Path__"]).name
                tracker.track(await self.driver.local() / "bin" / bin_path)

        await super().install(tracker=tracker)
