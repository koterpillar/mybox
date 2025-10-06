import dataclasses
from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable
from typing import TYPE_CHECKING, Any, Generic, Type, TypeVar

from .db import DB

if TYPE_CHECKING:
    from _typeshed import DataclassInstance  # pragma: no cover

    M = TypeVar("M", bound=DataclassInstance)  # pragma: no cover
else:
    M = TypeVar("M")


class Storage(Generic[M], ABC):
    @abstractmethod
    def __getitem__(self, key: str) -> M:
        pass

    @abstractmethod
    def __setitem__(self, key: str, value: M) -> None:
        pass

    @abstractmethod
    def find_ids(self, **kwargs: Any) -> Iterable[tuple[str, M]]:
        pass

    def find(self, **kwargs: Any) -> Iterable[M]:
        for _, value in self.find_ids(**kwargs):
            yield value


StorageDefinition = Callable[[DB], Storage[M]]


def storage(name: str, klass: Type[M]) -> StorageDefinition[M]:
    attributes: list[str] = [field.name for field in dataclasses.fields(klass)]

    def storage_table(db: DB) -> Storage[M]:
        with db as connection:
            connection.execute(
                f'CREATE TABLE IF NOT EXISTS {name} (id TEXT PRIMARY KEY, {", ".join(attributes)})'
            )

        class StorageImpl(Storage[M]):
            def find_ids(self, **kwargs: Any) -> Iterable[tuple[str, M]]:
                where = "1=1"
                values = []
                for key, value in kwargs.items():
                    where += f" AND {key} = ?"
                    values.append(value)

                with db as connection:
                    cursor = connection.execute(
                        f"SELECT * FROM {name} WHERE {where}", values
                    )

                    while rows := cursor.fetchmany():
                        for row in rows:
                            attributes = {
                                key: row[key] for key in row.keys() if key != "id"
                            }
                            yield row["id"], klass(**attributes)

            def __getitem__(self, key: str) -> M:
                items = self.find(id=key)
                try:
                    return next(iter(items))
                except StopIteration:
                    raise KeyError(key) from None

            def __setitem__(self, key: str, value: M) -> None:
                with db as connection:
                    connection.execute(f"DELETE FROM {name} WHERE id=?", [key])
                    attr_clause = "?"
                    attr_values = [key]
                    for attribute in attributes:
                        attr_clause += ", ?"
                        attr_values.append(getattr(value, attribute))
                    connection.execute(
                        f"INSERT INTO {name} VALUES ({attr_clause})", attr_values
                    )

        return StorageImpl()

    return storage_table
