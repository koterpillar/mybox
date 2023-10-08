import shlex
import tempfile
from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncIterator

import pytest

from mybox.driver import LocalDriver
from mybox.extractor import Tar, get_extractor
from mybox.utils import run


@asynccontextmanager
async def temporary_zip_archive(*paths: str) -> AsyncIterator[Path]:
    with tempfile.NamedTemporaryFile(suffix=".zip") as archive_file:
        archive = Path(archive_file.name)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmppath = Path(tmpdir)

            for element in paths:
                (tmppath / element).parent.mkdir(parents=True, exist_ok=True)
                (tmppath / element).touch()

            await run("rm", archive)
            await run(
                "sh",
                "-c",
                f"cd {shlex.quote(str(tmppath))} && zip -qq -r {shlex.quote(str(archive))} .",
            )

            yield archive


async def extract_file_names(archive: Path, *, strip: int = 0) -> set[str]:
    extractor = await get_extractor(str(archive), driver=LocalDriver())

    with tempfile.TemporaryDirectory() as dest:
        dest_path = Path(dest)
        await extractor.extract(
            archive=archive, target_directory=dest_path, strip=strip
        )

        return set(
            str(element.relative_to(dest_path)) for element in dest_path.iterdir()
        )


@pytest.mark.trio
async def test_unzip_strip():
    async with temporary_zip_archive("foo/bar", "foo/baz") as archive:
        assert await extract_file_names(archive, strip=1) == {"bar", "baz"}


@pytest.mark.trio
async def test_unzip_strip_multiple_elements():
    async with temporary_zip_archive("foo/bar", "foo/baz") as archive:
        with pytest.raises(
            ValueError, match="Expected exactly one item after extracting"
        ):
            await extract_file_names(archive, strip=2)


@pytest.mark.trio
async def test_unzip_strip_not_directory():
    async with temporary_zip_archive("foo") as archive:
        with pytest.raises(ValueError, match="Expected directory after extracting"):
            await extract_file_names(archive, strip=1)


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
