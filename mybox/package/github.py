import json
import os
from dataclasses import dataclass
from subprocess import CalledProcessError
from typing import Any, Iterator, Optional

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
    except KeyError:
        pass

    try:
        return await run_output("gh", "auth", "token", silent=True)
    except (CalledProcessError, FileNotFoundError):
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
    draft: bool
    prerelease: bool
    assets: list[GitHubReleaseArtifact]


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
        return [
            GitHubRelease(
                id=release["id"],
                tag_name=release["tag_name"],
                draft=release["draft"],
                prerelease=release["prerelease"],
                assets=[
                    GitHubReleaseArtifact(
                        name=result["name"], url=result["browser_download_url"]
                    )
                    for result in release["assets"]
                ],
            )
            for release in result
        ]

    async def release(self) -> GitHubRelease:
        candidates = await self.releases()
        for candidate in candidates:
            if candidate.draft:
                # Can't find any stably draft releases to test with
                continue  # pragma: no cover
            if candidate.prerelease:
                continue
            if candidate.tag_name in self.skip_releases:
                continue
            return candidate
        raise ValueError(f"No releases found for {self.repo}.")

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
