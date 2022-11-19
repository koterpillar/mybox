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


def test_storage_find(tmp_path: Path) -> None:
    widgets = make_widgets(tmp_path)
    widgets["one"] = Widget("red", 10)
    widgets["two"] = Widget("blue", 20)
    widgets["three"] = Widget("red", 30)

    widgets = make_widgets(tmp_path)
    items = list(widgets.find(color="red"))
    assert items == [Widget("red", 10), Widget("red", 30)]


def test_storage_append(tmp_path: Path) -> None:
    widgets = make_widgets(tmp_path)
    widgets.append(Widget("red", 10))

    widgets = make_widgets(tmp_path)
    items = list(widgets.find())
    assert items == [Widget("red", 10)]


def test_storage_delete(tmp_path: Path) -> None:
    widgets = make_widgets(tmp_path)
    widgets["one"] = Widget("red", 10)
    widgets["two"] = Widget("blue", 20)
    widgets["three"] = Widget("red", 30)

    widgets = make_widgets(tmp_path)
    widgets.delete(color="red")
    items = list(widgets.find())
    assert items == [Widget("blue", 20)]
