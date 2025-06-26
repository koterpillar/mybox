import json
import os
from collections.abc import Iterator
from dataclasses import dataclass
from subprocess import CalledProcessError
from typing import Any, Optional

import httpx
from pydantic import Field

from ..driver import OS, Architecture
from ..filters import Filter, Filters, choose
from ..utils import (
    allow_singular_none,
    async_cached,
    async_cached_lock,
    http_get,
    run_output,
)
from .archive import ArchivePackage


@async_cached_lock
async def github_auth_token() -> Optional[str]:
    try:
        return os.environ["GITHUB_TOKEN"]
    except KeyError:  # pragma: no cover
        pass

    try:  # pragma: no cover
        return await run_output("gh", "auth", "token", silent=True)
    except (CalledProcessError, FileNotFoundError):  # pragma: no cover
        pass

    return None  # pragma: no cover


async def github_api(url: str) -> Any:
    token = await github_auth_token()

    headers = {}
    if token:
        headers["Authorization"] = f"token {token}"

    result = await http_get(f"https://api.github.com/{url}", headers=headers)

    return json.loads(result)


@dataclass
class GitHubReleaseArtifact:
    name: str
    url: str


@dataclass
class GitHubRelease:
    id: int
    tag_name: str
    prerelease: bool
    assets: list[GitHubReleaseArtifact]

    @classmethod
    def from_json(cls, data: dict[str, Any]) -> "GitHubRelease":
        return cls(
            id=data["id"],
            tag_name=data["tag_name"],
            prerelease=data["prerelease"],
            assets=[
                GitHubReleaseArtifact(
                    name=result["name"], url=result["browser_download_url"]
                )
                for result in data["assets"]
            ],
        )


ARCHITECTURE_FILTERS: dict[str, list[str]] = {
    "aarch64": ["arm64", "arm"],
    "i386": ["i686", "x86"],
    "mips": [],
    "powerpc": ["ppc"],
    "s390x": [],
    "x86_64": ["amd64", "x64"],
}

OS_FILTERS: dict[str, list[str]] = {
    "darwin": ["macOS", "macos", "osx"],
    "linux": [],
    "windows": [],
}


class GitHubPackage(ArchivePackage, Filters):
    repo: str
    skip_releases: list[str] = Field(default_factory=list, alias="skip_release")
    skip_releases_val = allow_singular_none("skip_releases")

    @async_cached
    async def releases(self) -> list[GitHubRelease]:
        result = await github_api(f"repos/{self.repo}/releases")
        return [GitHubRelease.from_json(release) for release in result]

    @async_cached
    async def latest_release(self) -> Optional[GitHubRelease]:
        try:
            result = await github_api(f"repos/{self.repo}/releases/latest")
        except httpx.HTTPStatusError as e:
            if e.response.status_code == 404:
                return None
            raise  # pragma: no cover
        return GitHubRelease.from_json(result)

    def want_release(self, release: GitHubRelease) -> bool:
        if release.prerelease:
            return False
        if release.tag_name in self.skip_releases:
            return False
        return True

    async def release(self) -> GitHubRelease:
        latest = await self.latest_release()
        if latest and self.want_release(latest):
            return latest

        candidates = await self.releases()
        for candidate in candidates:
            if not self.want_release(candidate):
                continue
            return candidate
        raise ValueError(f"No releases found for {self.repo}.")

    @classmethod
    def environment_filters(
        cls, *, target_os: OS, target_arch: Architecture
    ) -> Iterator[Filter[str]]:
        for signature_hint in [".asc", ".sig", "sha256", "sha512", ".yml"]:
            yield cls.excludes_(signature_hint)

        for system_package_hint in [".deb", ".rpm", ".dmg", ".exe", ".appimage"]:
            yield cls.excludes_(system_package_hint)

        yield from cls.from_synonyms(
            OS_FILTERS, target_os.switch(linux="linux", macos="darwin")
        )

        yield from cls.from_synonyms(ARCHITECTURE_FILTERS, target_arch)

        if target_os.switch(linux=True, macos=False):
            yield cls.includes_("gnu")
            yield cls.excludes_("musl")  # pragma: no cover

    def all_filters(
        self, *, target_os: OS, target_arch: Architecture
    ) -> Iterator[Filter[str]]:
        yield from self.filters()
        yield from self.environment_filters(
            target_os=target_os, target_arch=target_arch
        )

    async def artifact(self) -> GitHubReleaseArtifact:
        candidates = (await self.release()).assets

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
        release = await self.release()
        return str(release.id)
