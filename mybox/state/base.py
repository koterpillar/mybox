import dataclasses
import sqlite3
import threading
from abc import ABCMeta, abstractmethod
from typing import Callable, Generic, Type, TypeVar

T = TypeVar("T")

DB_PATH = "state.sqlite"


DB_CACHE = threading.local()


DB = sqlite3.Connection


def make_db() -> DB:
    try:
        return DB_CACHE.connection
    except AttributeError:
        pass
    connection = sqlite3.connect(DB_PATH)
    connection.row_factory = sqlite3.Row
    DB_CACHE.connection = connection
    return connection


class Table(Generic[T], metaclass=ABCMeta):
    @abstractmethod
    def __getitem__(self, key: str) -> T:
        pass

    @abstractmethod
    def __delitem__(self, key: str) -> None:
        pass

    @abstractmethod
    def __setitem__(self, key: str, value: T) -> None:
        pass


StorageTable = Callable[[DB], Table[T]]


def storage(name: str, klass: Type[T]) -> StorageTable[T]:
    attributes: list[str] = [field.name for field in dataclasses.fields(klass)]

    def storage_table(db: DB) -> Table[T]:
        db.execute(
            f'CREATE TABLE IF NOT EXISTS {name} (id TEXT PRIMARY KEY, {", ".join(attributes)})'
        )

        class Storage(Table[T]):
            def __getitem__(self, key: str) -> T:
                row = db.execute(
                    f"SELECT * FROM {name} WHERE id = ?", (key,)
                ).fetchone()
                if row:
                    attributes = {key: row[key] for key in row.keys() if key != "id"}
                    return klass(**attributes)
                raise KeyError(key)

            def __delitem__(self, key: str) -> None:
                db.execute(f"DELETE FROM {name} WHERE id = ?", (key,))

            def __setitem__(self, key: str, value: T) -> None:
                with db:
                    del self[key]
                    attr_clause = "?"
                    attr_values = [key]
                    for attribute in attributes:
                        attr_clause += ", ?"
                        attr_values.append(getattr(value, attribute))
                    db.execute(
                        f"INSERT INTO {name} VALUES ({attr_clause})", attr_values
                    )

        return Storage()

    return storage_table
