import json
import os
from dataclasses import dataclass
from typing import Any, Callable, Iterator

import requests

from ..driver import OS
from ..utils import Filters, Some, async_cached, choose, run_ok, run_output, unsome
from .archive import ArchivePackage


@async_cached
async def have_github_auth() -> bool:
    return await run_ok("gh", "auth", "status")


async def github_api(url: str) -> Any:
    if await have_github_auth():
        return json.loads(await run_output("gh", "api", url))
    else:
        try:
            token = os.environ["GITHUB_TOKEN"]
        except KeyError:
            token = None

        headers = {}
        if token:
            headers["Authorization"] = f"token {token}"

        result = requests.get(f"https://api.github.com/{url}", headers=headers)
        result.raise_for_status()

        return result.json()


@dataclass
class GitHubReleaseArtifact:
    name: str
    url: str


@dataclass
class GitHubRelease:
    tag_name: str
    assets: list[GitHubReleaseArtifact]


class GitHubPackage(ArchivePackage):
    prefixes: list[str]
    suffixes: list[str]
    includes: list[str]
    excludes: list[str]

    def __init__(
        self,
        *,
        repo: str,
        prefix: Some[str] = None,
        suffix: Some[str] = None,
        include: Some[str] = None,
        exclude: Some[str] = None,
        regex: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.repo = repo
        self.prefixes = unsome(prefix)
        self.suffixes = unsome(suffix)
        self.includes = unsome(include)
        self.excludes = unsome(exclude)
        self.regex = unsome(regex)

    @async_cached
    async def latest_release(self) -> GitHubRelease:
        latest = await github_api(f"repos/{self.repo}/releases/latest")
        return GitHubRelease(
            tag_name=latest["tag_name"],
            assets=[
                GitHubReleaseArtifact(
                    name=result["name"], url=result["browser_download_url"]
                )
                for result in latest["assets"]
            ],
        )

    def filters(self, target_os: OS) -> Iterator[Callable[[str], bool]]:
        for prefix in self.prefixes:
            yield Filters.startswith(prefix)
        for suffix in self.suffixes:
            yield Filters.endswith(suffix)
        for include in self.includes:
            yield Filters.includes(include)
        for exclude in self.excludes:
            yield Filters.excludes(exclude)
        for regex in self.regex:
            yield Filters.regex(regex)
        for hint in [".tar.gz"]:
            yield Filters.includes(hint)
        for signature_hint in [".asc", ".sig", "sha256", "sha512", ".yml"]:
            yield Filters.excludes(signature_hint)
        for other_os_hint in [".exe", ".dmg"]:
            yield Filters.excludes(other_os_hint)
        for os_hint in target_os.switch(
            linux=[
                Filters.includes("linux"),
                Filters.includes("gnu"),
                Filters.excludes("musl"),
            ],
            macos=[Filters.includes(hint) for hint in ["macos", "darwin", "osx"]],
        ):
            yield os_hint
        arch_hints = ["x86_64", "amd64"]
        for hint in arch_hints:
            yield Filters.includes(hint)

    async def artifact(self) -> GitHubReleaseArtifact:
        candidates = (await self.latest_release()).assets

        def candidate_filter(
            name_filter: Callable[[str], bool]
        ) -> Callable[[GitHubReleaseArtifact], bool]:
            return lambda candidate: name_filter(candidate.name)

        target_os = await self.driver.os()
        return choose(candidates, map(candidate_filter, self.filters(target_os)))

    async def archive_url(self) -> str:
        return (await self.artifact()).url

    @property
    def name(self) -> str:
        return self.repo

    async def get_remote_version(self) -> str:
        return (await self.latest_release()).tag_name
