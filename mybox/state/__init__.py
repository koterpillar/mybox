import dataclasses
import sqlite3
import threading
from abc import ABCMeta, abstractmethod
from typing import Callable, Generic, Type, TypeVar

T = TypeVar("T")

DB_PATH = "state.sqlite"


class DB:
    """Thread-safe SQLite database connection."""

    def __init__(self, path: str) -> None:
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


StorageDefinition = Callable[[DB], Storage[T]]


def storage(name: str, klass: Type[T]) -> StorageDefinition[T]:
    attributes: list[str] = [field.name for field in dataclasses.fields(klass)]

    def storage_table(db: DB) -> Storage[T]:
        db.instance.execute(
            f'CREATE TABLE IF NOT EXISTS {name} (id TEXT PRIMARY KEY, {", ".join(attributes)})'
        )

        class StorageImpl(Storage[T]):
            def __getitem__(self, key: str) -> T:
                row = db.instance.execute(
                    f"SELECT * FROM {name} WHERE id = ?", (key,)
                ).fetchone()
                if row:
                    attributes = {key: row[key] for key in row.keys() if key != "id"}
                    return klass(**attributes)
                raise KeyError(key)

            def __delitem__(self, key: str) -> None:
                db.instance.execute(f"DELETE FROM {name} WHERE id = ?", (key,))

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

        return StorageImpl()

    return storage_table
