import json
import re
from pathlib import Path
from typing import Any, Optional, cast

from pydantic import Field, field_validator

from ..tracker import Tracker
from ..utils import GIT_PREFIX, repo_version
from .manual_version import ManualVersion


class PipxPackage(ManualVersion):
    package: str = Field(..., alias="pipx")

    @field_validator("package")
    @classmethod
    def package_to_lower(cls, value: str) -> str:
        return value.lower()

    def derive_name(self) -> str:
        return self.package

    async def get_metadata(self) -> Optional[dict[str, Any]]:
        pipx_list = json.loads(
            await self.driver.run_output("pipx", "list", "--json", silent=True)
        )
        packages = {
            k: v["metadata"]["main_package"] for k, v in pipx_list["venvs"].items()
        }
        if self.is_repo:
            return next(
                (
                    pkg
                    for pkg in packages.values()
                    if pkg["package_or_url"] == self.package
                ),
                None,
            )
        else:
            return packages.get(self.package)

    @property
    def is_repo(self) -> bool:
        return self.package.startswith(GIT_PREFIX)

    async def local_version(self) -> Optional[str]:
        if self.is_repo:
            # pipx doesn't store Git commit, just the version field from the package
            return self.cached_version

        metadata = await self.get_metadata()
        if metadata:
            return metadata["package_version"]
        else:
            return None

    async def get_remote_version(self) -> str:
        if self.is_repo:
            return await repo_version(self.package)

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
        if self.is_repo:
            # https://github.com/pypa/pipx/issues/892
            cmd = ("install", "--force")
        else:
            cmd = ("upgrade", "--install")

        await self.driver.run("pipx", *cmd, self.package)

        tracker.track(await self.driver.local() / "pipx" / "venvs" / self.package)
        metadata = await self.get_metadata()
        if metadata:
            for bin_desc in metadata["app_paths"]:
                bin_path = Path(bin_desc["__Path__"]).name
                tracker.track(await self.driver.local() / "bin" / bin_path)

        await super().install(tracker=tracker)

        if self.is_repo:
            await self.cache_version()
