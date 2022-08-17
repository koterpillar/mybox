from dataclasses import dataclass

from .base import storage


@dataclass
class Version:
    version: str


VERSIONS = storage("version", Version)
