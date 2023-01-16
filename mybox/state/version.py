from dataclasses import dataclass

from .base import storage


@dataclass(frozen=True)
class Version:
    version: str


VERSIONS = storage("version", Version)
