import shlex
from typing import AsyncIterable

from pydantic import Field

from ..tracker import Tracker
from ..utils import allow_singular_none
from .base import Package
from .manual_version import ManualVersion
from .system import SystemPackage


class NpmPackage(ManualVersion):
    package: str = Field(..., alias="npm")

    binaries: list[str] = Field(default_factory=list, alias="binary")
    binaries_val = allow_singular_none("binaries")

    def derive_name(self) -> str:
        return self.package

    async def get_remote_version(self) -> str:
        try:
            result = await self.driver.run_output(
                "npm", "view", self.package, "version"
            )
        except Exception as exc:
            raise Exception(
                f"Cannot find latest version of package '{self.package}'."
            ) from exc
        version = result.strip()
        return version

    async def install(self, *, tracker: Tracker) -> None:
        args = ["npm", "exec", "--yes", "--package", self.package, "--"]

        await self.driver.run(*args, "true")

        npx_paths = await self.driver.run_output(*args, "echo $PATH")
        npx_path = next((path for path in npx_paths.split(":") if "_npx" in path), None)
        if not npx_path:
            raise Exception(
                f"Could not find npx path in {npx_paths}."
            )  # pragma: no cover

        for name in self.binaries:
            target = await self.driver.local() / "bin" / name
            await self.driver.write_file(
                target,
                f'#!/bin/sh\nPATH={shlex.quote(npx_path)}:$PATH\nexec "{name}" "$@"',
            )
            await self.driver.make_executable(target)
            tracker.track(target, root=self.root)

        await super().install(tracker=tracker)

    async def prerequisites(self) -> AsyncIterable[Package]:
        async for package in super().prerequisites():
            yield package  # pragma: no cover

        os = await self.driver.os()

        for system in os.switch_(
            linux=lambda linux: [
                "nodejs",
                {
                    "debian": "npm",
                    "ubuntu": "npm",
                    "fedora": "nodejs-npm",
                }[linux.distribution],
            ],
            macos=lambda: ["node"],
        ):
            yield SystemPackage(
                system=system,
                db=self.db,
                driver=self.driver_,
            )
