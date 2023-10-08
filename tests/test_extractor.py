import shlex
import tempfile
from pathlib import Path

import pytest

from mybox.driver import LocalDriver
from mybox.extractor import Tar, get_extractor
from mybox.utils import run


@pytest.mark.trio
async def test_unzip_strip():
    with tempfile.NamedTemporaryFile(suffix=".zip") as archive_file:
        archive = Path(archive_file.name)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            (tmppath / "foo").mkdir()
            (tmppath / "foo" / "bar").touch()
            (tmppath / "foo" / "baz").touch()

            await run("rm", archive)
            await run(
                "sh",
                "-c",
                f"cd {shlex.quote(str(tmppath))} && zip -qq -r {shlex.quote(str(archive))} .",
            )

        extractor = await get_extractor(str(archive), driver=LocalDriver())

        with tempfile.TemporaryDirectory() as dest:
            dest_path = Path(dest)
            await extractor.extract(
                archive=archive, target_directory=dest_path, strip=1
            )

            assert set(dest_path.iterdir()) == {dest_path / "bar", dest_path / "baz"}


@pytest.mark.trio
async def test_get_extractor_redirect():
    extractor = await get_extractor(
        "https://telegram.org/dl/desktop/linux", driver=LocalDriver()
    )
    assert isinstance(extractor, Tar)
    assert extractor.extra == ["-J"]


@pytest.mark.trio
async def test_get_extractor_unknown():
    with pytest.raises(
        ValueError, match="Unknown archive format: https://example.com/"
    ):
        await get_extractor("https://example.com", driver=LocalDriver())
