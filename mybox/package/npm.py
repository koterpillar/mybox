import shlex

import requests
from pydantic import Field

from ..utils import allow_singular_none
from .manual_version import ManualVersion
from .root import Root
from .tracked import Tracked, Tracker


class NpmPackage(Root, ManualVersion, Tracked):
    package: str = Field(..., alias="npm")

    binaries: list[str] = Field(default_factory=list, alias="binary")
    binaries_val = allow_singular_none("binaries")

    def derive_name(self) -> str:
        return self.package

    async def get_remote_version(self) -> str:
        result = requests.get(f"https://registry.npmjs.com/{self.package}")
        result.raise_for_status()

        details = result.json()
        return details["dist-tags"]["latest"]

    async def install_tracked(self, *, tracker: Tracker) -> None:
        args = ["npm", "exec", "--yes", "--package", self.package, "--"]

        await self.driver.run(*args, "true")

        npx_paths = await self.driver.run_output(*args, "echo $PATH")
        npx_path = next((path for path in npx_paths.split(":") if "_npx" in path), None)
        if not npx_path:
            raise Exception(f"Could not find npx path in {npx_paths}.")

        for name in self.binaries:
            target = await self.local() / "bin" / name
            await self.driver.write_file(
                target,
                f'#!/bin/sh\nPATH={shlex.quote(npx_path)}:$PATH\nexec "{name}" "$@"',
            )
            await self.driver.make_executable(target)
            tracker.track(target)

        await super().install_tracked(tracker=tracker)
