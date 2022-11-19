import dataclasses
import sqlite3
import threading
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Any, Callable, Generic, Iterable, Tuple, Type, TypeVar, Union
from uuid import uuid4

T = TypeVar("T")

DB_PATH = "state.sqlite"


class DB:
    """Thread-safe SQLite database connection."""

    def __init__(self, path: Union[str, Path]) -> None:
        self.path = path
        self.cache = threading.local()

    @property
    def instance(self) -> sqlite3.Connection:
        try:
            return self.cache.connection
        except AttributeError:
            pass

        connection = sqlite3.connect(self.path)
        connection.row_factory = sqlite3.Row
        self.cache.connection = connection
        return connection


class Storage(Generic[T], metaclass=ABCMeta):
    @abstractmethod
    def __getitem__(self, key: str) -> T:
        pass

    @abstractmethod
    def __delitem__(self, key: str) -> None:
        pass

    @abstractmethod
    def __setitem__(self, key: str, value: T) -> None:
        pass

    @abstractmethod
    def append(self, value: T) -> None:
        pass

    @abstractmethod
    def find(self, **kwargs: Any) -> Iterable[T]:
        pass

    @abstractmethod
    def delete(self, **kwargs: Any) -> None:
        pass


StorageDefinition = Callable[[DB], Storage[T]]


def storage(name: str, klass: Type[T]) -> StorageDefinition[T]:
    attributes: list[str] = [field.name for field in dataclasses.fields(klass)]

    def storage_table(db: DB) -> Storage[T]:
        db.instance.execute(
            f'CREATE TABLE IF NOT EXISTS {name} (id TEXT PRIMARY KEY, {", ".join(attributes)})'
        )

        class StorageImpl(Storage[T]):
            def where_clause(self, **kwargs: Any) -> Tuple[str, list[Any]]:
                result = "1=1"
                values = []
                for key, value in kwargs.items():
                    result += f" AND {key} = ?"
                    values.append(value)
                return result, values

            def find(self, **kwargs: Any) -> Iterable[T]:
                where, values = self.where_clause(**kwargs)

                cursor = db.instance.execute(
                    f"SELECT * FROM {name} WHERE {where}", values
                )

                while rows := cursor.fetchmany():
                    for row in rows:
                        attributes = {
                            key: row[key] for key in row.keys() if key != "id"
                        }
                        yield klass(**attributes)

            def __getitem__(self, key: str) -> T:
                items = self.find(id=key)
                try:
                    return next(iter(items))
                except StopIteration:
                    raise KeyError(key) from None

            def __delitem__(self, key: str) -> None:
                self.delete(id=key)

            def delete(self, **kwargs: Any) -> None:
                where, values = self.where_clause(**kwargs)
                db.instance.execute(f"DELETE FROM {name} WHERE {where}", values)

            def __setitem__(self, key: str, value: T) -> None:
                with db.instance:
                    del self[key]
                    attr_clause = "?"
                    attr_values = [key]
                    for attribute in attributes:
                        attr_clause += ", ?"
                        attr_values.append(getattr(value, attribute))
                    db.instance.execute(
                        f"INSERT INTO {name} VALUES ({attr_clause})", attr_values
                    )

            def append(self, value: T) -> None:
                self[str(uuid4())] = value

        return StorageImpl()

    return storage_table
