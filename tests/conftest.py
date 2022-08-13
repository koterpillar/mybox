import tempfile
from pathlib import Path
from typing import Iterator

import pytest

pytest.register_assert_rewrite("tests.package.base")


@pytest.fixture
def override_home() -> Iterator[Path]:
    with tempfile.TemporaryDirectory(prefix="mybox_home_") as tmpdir:
        yield Path(tmpdir)
