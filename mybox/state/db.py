import sqlite3
import threading
from pathlib import Path
from types import TracebackType

DB_PATH = "state.sqlite"


class DB:
    """Thread-safe SQLite database connection."""

    @classmethod
    def temporary(cls) -> "DB":
        return cls(":memory:", close=False)

    def __init__(self, path: str | Path, close: bool = True) -> None:
        self.path = path
        self.cache = threading.local()
        self.close = close

    def _make_connection(self) -> sqlite3.Connection:
        connection = sqlite3.connect(self.path)
        connection.row_factory = sqlite3.Row
        return connection

    @property
    def _connection(self) -> sqlite3.Connection:
        if not hasattr(self.cache, "connection"):
            self.cache.connection = self._make_connection()
        return self.cache.connection

    @property
    def _depth(self) -> int:
        return getattr(self.cache, "depth", 0)

    def _depth_alter(self, delta: int) -> int:
        self.cache.depth = self._depth + delta
        return self.cache.depth

    def __enter__(self) -> sqlite3.Connection:
        self._depth_alter(1)
        return self._connection.__enter__()

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        self._connection.__exit__(exc_type, exc_val, exc_tb)
        if self._depth_alter(-1) == 0:
            if self.close:
                self._connection.close()
                delattr(self.cache, "connection")
