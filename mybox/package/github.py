import json
import os
import shutil
from dataclasses import dataclass
from functools import cache
from typing import Any, Callable, Iterator

import requests

from ..utils import *
from .archive import ArchivePackage


@cache
def have_github_auth() -> bool:
    return bool(shutil.which("gh")) and run_ok("gh", "auth", "status")


def github_api(url: str) -> Any:
    if have_github_auth():
        return json.loads(run_output("gh", "api", url))
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
    excludes: list[str]

    def __init__(
        self,
        *,
        repo: str,
        prefix: Some[str] = None,
        suffix: Some[str] = None,
        exclude: Some[str] = None,
        regex: Some[str] = None,
        **kwargs,
    ) -> None:
        self.repo = repo
        self.prefixes = unsome(prefix)
        self.suffixes = unsome(suffix)
        self.excludes = unsome(exclude)
        self.regex = unsome(regex)
        super().__init__(**kwargs)

    @cache
    def latest_release(self) -> GitHubRelease:
        latest = github_api(f"repos/{self.repo}/releases/latest")
        return GitHubRelease(
            tag_name=latest["tag_name"],
            assets=[
                GitHubReleaseArtifact(
                    name=result["name"], url=result["browser_download_url"]
                )
                for result in latest["assets"]
            ],
        )

    def filters(self) -> Iterator[Callable[[str], bool]]:
        for prefix in self.prefixes:
            yield Filters.startswith(prefix)
        for suffix in self.suffixes:
            yield Filters.endswith(suffix)
        for exclude in self.excludes:
            yield Filters.excludes(exclude)
        for regex in self.regex:
            yield Filters.regex(regex)
        for hint in [".tar.gz"]:
            yield Filters.includes(hint)
        for signature_hint in [".asc", ".sig", "sha256sum"]:
            yield Filters.excludes(signature_hint)
        os_hints = with_os(
            linux=["linux", "gnu"],
            macos=["macos", "darwin", "osx"],
        )
        for hint in os_hints:
            yield Filters.includes(hint)
        if CURRENT_OS == "linux":
            yield Filters.excludes("musl")
        arch_hints = ["x86_64", "amd64"]
        for hint in arch_hints:
            yield Filters.includes(hint)

    def artifact(self) -> GitHubReleaseArtifact:
        candidates = self.latest_release().assets

        def candidate_filter(
            name_filter: Callable[[str], bool]
        ) -> Callable[[GitHubReleaseArtifact], bool]:
            return lambda candidate: name_filter(candidate.name)

        return choose(candidates, map(candidate_filter, self.filters()))

    def archive_url(self) -> str:
        return self.artifact().url

    @property
    def name(self) -> str:
        return self.repo

    def get_remote_version(self) -> str:
        return self.latest_release().tag_name
