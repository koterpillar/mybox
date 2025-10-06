from dataclasses import dataclass
from typing import Generic

from .utils import T


@dataclass
class PartialSuccess(Generic[T]):
    result: T


@dataclass
class PartialException:
    exception: BaseException


PartialResult = PartialException | PartialSuccess[T]


@dataclass
class PartialResults(Exception, Generic[T]):
    results: list[PartialResult[T]]
