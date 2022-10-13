from dataclasses import dataclass

import pytest

from mybox.state import DB, Storage, storage


@dataclass
class Widget:
    color: str
    size: int


WIDGETS = storage("widget", Widget)


def test_storage(tmp_path) -> None:
    def make_widgets() -> Storage[Widget]:
        db = DB(tmp_path / "temp.sqlite")
        return WIDGETS(db)

    widgets = make_widgets()
    widgets["one"] = Widget("red", 10)
    widgets["two"] = Widget("blue", 20)

    widgets = make_widgets()
    one = widgets["one"]
    assert one.color == "red"
    assert one.size == 10

    with pytest.raises(KeyError):
        widgets["three"]  # pylint:disable=pointless-statement
