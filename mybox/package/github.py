import os
from dataclasses import dataclass
from subprocess import CalledProcessError
from typing import Any, Iterator, Optional

import requests

from ..driver import OS, Architecture
from ..filters import Filter, Filters, choose
from ..utils import async_cached, async_cached_lock, run_output
from .archive import ArchivePackage


@async_cached_lock
async def github_auth_token() -> Optional[str]:
    try:
        return os.environ["GITHUB_TOKEN"]
    except KeyError:
        pass

    try:
        return await run_output("gh", "auth", "token", silent=True)
    except (CalledProcessError, FileNotFoundError):
        pass

    return None


async def github_api(url: str) -> Any:
    token = await github_auth_token()

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
    id: int
    tag_name: str
    assets: list[GitHubReleaseArtifact]


ARCHITECTURE_FILTERS: dict[str, list[str]] = {
    "arm64": ["aarch64", "arm"],
    "i386": ["i686", "x86"],
    "mips": [],
    "powerpc": ["ppc"],
    "s390x": [],
    "x86_64": ["amd64", "x64"],
}

OS_FILTERS: dict[str, list[str]] = {
    "darwin": ["macos", "osx"],
    "linux": [],
    "windows": [],
}


class GitHubPackage(ArchivePackage, Filters):
    repo: str

    @async_cached
    async def latest_release(self) -> GitHubRelease:
        latest = await github_api(f"repos/{self.repo}/releases/latest")
        return GitHubRelease(
            id=latest["id"],
            tag_name=latest["tag_name"],
            assets=[
                GitHubReleaseArtifact(
                    name=result["name"], url=result["browser_download_url"]
                )
                for result in latest["assets"]
            ],
        )

    @classmethod
    def environment_filters(
        cls, *, target_os: OS, target_arch: Architecture
    ) -> Iterator[Filter[str]]:
        for signature_hint in [".asc", ".sig", "sha256", "sha512", ".yml"]:
            yield cls.excludes_(signature_hint)

        for system_package_hint in [".deb", ".rpm", ".dmg", ".exe"]:
            yield cls.excludes_(system_package_hint)

        yield from cls.from_synonyms(
            OS_FILTERS, target_os.switch(linux="linux", macos="darwin")
        )

        yield from cls.from_synonyms(ARCHITECTURE_FILTERS, target_arch)

        if target_os.switch(linux=True, macos=False):
            yield cls.includes_("gnu")
            yield cls.excludes_("musl")

    def all_filters(
        self, *, target_os: OS, target_arch: Architecture
    ) -> Iterator[Filter[str]]:
        yield from self.filters()
        yield from self.environment_filters(
            target_os=target_os, target_arch=target_arch
        )

    async def artifact(self) -> GitHubReleaseArtifact:
        candidates = (await self.latest_release()).assets

        def candidate_filter(
            name_filter: Filter,
        ) -> Filter[GitHubReleaseArtifact]:
            return lambda candidate: name_filter(candidate.name)

        target_os = await self.driver.os()
        target_arch = await self.driver.architecture()
        return choose(
            candidates,
            map(
                candidate_filter,
                self.all_filters(target_os=target_os, target_arch=target_arch),
            ),
        )

    async def archive_url(self) -> str:
        return (await self.artifact()).url

    def derive_name(self) -> str:
        return self.repo

    async def get_remote_version(self) -> str:
        release = await self.latest_release()
        return str(release.id)
