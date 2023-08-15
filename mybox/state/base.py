import dataclasses
from abc import ABCMeta, abstractmethod
from typing import TYPE_CHECKING, Any, Callable, Generic, Iterable, Type, TypeVar
from uuid import uuid4

from .db import DB

if TYPE_CHECKING:
    from _typeshed import DataclassInstance  # pragma: no cover

    M = TypeVar("M", bound=DataclassInstance)  # pragma: no cover
else:
    M = TypeVar("M")


class Storage(Generic[M], metaclass=ABCMeta):
    @abstractmethod
    def __getitem__(self, key: str) -> M:
        pass

    @abstractmethod
    def __delitem__(self, key: str) -> None:
        pass

    @abstractmethod
    def __setitem__(self, key: str, value: M) -> None:
        pass

    @abstractmethod
    def append(self, value: M) -> None:
        pass

    @abstractmethod
    def find_ids(self, **kwargs: Any) -> Iterable[tuple[str, M]]:
        pass

    def find(self, **kwargs: Any) -> Iterable[M]:
        for _, value in self.find_ids(**kwargs):
            yield value

    @abstractmethod
    def delete(self, **kwargs: Any) -> None:
        pass


StorageDefinition = Callable[[DB], Storage[M]]


def storage(name: str, klass: Type[M]) -> StorageDefinition[M]:
    attributes: list[str] = [field.name for field in dataclasses.fields(klass)]

    def storage_table(db: DB) -> Storage[M]:
        with db as connection:
            connection.execute(
                f'CREATE TABLE IF NOT EXISTS {name} (id TEXT PRIMARY KEY, {", ".join(attributes)})'
            )

        class StorageImpl(Storage[M]):
            def where_clause(self, **kwargs: Any) -> tuple[str, list[Any]]:
                result = "1=1"
                values = []
                for key, value in kwargs.items():
                    result += f" AND {key} = ?"
                    values.append(value)
                return result, values

            def find_ids(self, **kwargs: Any) -> Iterable[tuple[str, M]]:
                where, values = self.where_clause(**kwargs)

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

            def __delitem__(self, key: str) -> None:
                self.delete(id=key)

            def delete(self, **kwargs: Any) -> None:
                where, values = self.where_clause(**kwargs)
                with db as connection:
                    connection.execute(f"DELETE FROM {name} WHERE {where}", values)

            def __setitem__(self, key: str, value: M) -> None:
                with db as connection:
                    del self[key]
                    attr_clause = "?"
                    attr_values = [key]
                    for attribute in attributes:
                        attr_clause += ", ?"
                        attr_values.append(getattr(value, attribute))
                    connection.execute(
                        f"INSERT INTO {name} VALUES ({attr_clause})", attr_values
                    )

            def append(self, value: M) -> None:
                self[str(uuid4())] = value

        return StorageImpl()

    return storage_table
