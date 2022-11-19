from dataclasses import dataclass
from pathlib import Path

import pytest

from mybox.state import DB, Storage, storage


@dataclass
class Widget:
    color: str
    size: int


WIDGETS = storage("widget", Widget)


def make_widgets(path: Path) -> Storage[Widget]:
    db = DB(path / "temp.sqlite")
    return WIDGETS(db)


def test_storage_retrieve(tmp_path: Path) -> None:
    widgets = make_widgets(tmp_path)
    widgets["one"] = Widget("red", 10)

    widgets = make_widgets(tmp_path)
    one = widgets["one"]
    assert one.color == "red"
    assert one.size == 10


def test_storage_non_existent(tmp_path: Path) -> None:
    widgets = make_widgets(tmp_path)
    with pytest.raises(KeyError):
        widgets["three"]  # pylint:disable=pointless-statement
