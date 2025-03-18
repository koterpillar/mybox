from fnmatch import fnmatchcase
from functools import cached_property
from typing import Any, Optional, Sequence

from pydantic import BaseModel, ConfigDict, Field, TypeAdapter

from .driver import Driver
from .utils import (
    allow_singular,
    allow_singular_none,
)


class MatchConfig(BaseModel):
    model_config = ConfigDict(
        frozen=True,
        arbitrary_types_allowed=True,
        ignored_types=(cached_property,),
        extra="forbid",
    )

    host: Optional[list[str]] = None
    host_val = allow_singular("host")

    component: list[str] = Field(default_factory=list)
    component_val = allow_singular_none("component")

    driver: Driver

    async def applicable(self) -> bool:
        if self.host is not None:
            host = await self.driver.run_output("hostname")
            if not any(fnmatchcase(host, pattern) for pattern in self.host):
                return False
        return True

    @staticmethod
    async def components(config: Sequence["MatchConfig"]) -> list[str]:
        result = []
        for match in config:
            if await match.applicable():
                result.extend(match.component)
        return result


MatchConfigList = TypeAdapter(list[MatchConfig])


def parse_config(config: Any, *, driver: Driver) -> list[MatchConfig]:
    config = TypeAdapter(list[dict]).validate_python(config)
    return MatchConfigList.validate_python(
        [{**match, "driver": driver} for match in config]
    )
