from pathlib import Path

import requests

from ..utils import Some, unsome
from .manual_version import ManualVersion
from .root import Root
from .tracked import Tracked, Tracker


class NpmPackage(Root, ManualVersion, Tracked):
    def __init__(self, npm: str, binary: Some[str] = None, **kwargs) -> None:
        super().__init__(**kwargs)
        self.package = npm
        self.binaries = unsome(binary)

    @property
    def name(self) -> str:
        return self.package

    async def get_remote_version(self) -> str:
        result = requests.get(f"https://registry.npmjs.com/{self.package}")
        result.raise_for_status()

        details = result.json()
        return details["dist-tags"]["latest"]

    async def install_tracked(self, *, tracker: Tracker) -> None:
        args = ["npm", "exec", "--package", self.package]

        await self.driver.run(*args, "true")

        npx_path = await self.driver.run_output(*args, "echo $PATH")
        binaries_path = Path(npx_path.split(":", maxsplit=1)[0])

        for name in self.binaries:
            binary = binaries_path / name
            target = await self.local() / "bin" / name
            await self.driver.link(Path(binary), target)
            tracker.track(target)

        await super().install_tracked(tracker=tracker)
