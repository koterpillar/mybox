import shlex
import tempfile
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from pathlib import Path

import pytest

from mybox.driver import LocalDriver
from mybox.extractor import Tar, get_extractor, get_single_extractor
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


async def extract_file_names(archive: Path) -> set[str]:
    extractor = await get_extractor(str(archive), driver=LocalDriver())

    with tempfile.TemporaryDirectory() as dest:
        dest_path = Path(dest)
        await extractor.extract(archive=archive, target_directory=dest_path)

        return set(
            str(element.relative_to(dest_path)) for element in dest_path.iterdir()
        )


@pytest.mark.trio
async def test_unzip_strip():
    async with temporary_zip_archive("foo/bar", "foo/baz") as archive:
        assert await extract_file_names(archive) == {"bar", "baz"}


@pytest.mark.trio
async def test_unzip_strip_multiple_elements():
    async with temporary_zip_archive("bar", "baz") as archive:
        assert await extract_file_names(archive) == {"bar", "baz"}


@pytest.mark.trio
async def test_unzip_strip_not_directory():
    async with temporary_zip_archive("foo/bar") as archive:
        assert await extract_file_names(archive) == {"bar"}


@pytest.mark.trio
async def test_unzip_strip_too_much_nesting():
    async with temporary_zip_archive("/".join(map(str, range(100)))) as archive:
        with pytest.raises(
            ValueError,
            match="Too many nested directories after extracting, got .+/0/1/2/3/4/5/6/7/8/9.",
        ):
            await extract_file_names(archive)


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


@pytest.mark.trio
@pytest.mark.parametrize(
    "compress_command,extension", [("gzip", "gz"), ("xz", "xz"), ("bzip2", "bz2")]
)
async def test_single_extractor(compress_command: str, extension: str):
    with tempfile.TemporaryDirectory() as srcdir:
        srcpath = Path(srcdir)
        (srcpath / "myfile").write_text("contents")
        await run(compress_command, srcpath / "myfile")

        archive = srcpath / f"myfile.{extension}"

        extractor = await get_single_extractor(str(archive), driver=LocalDriver())
        with tempfile.TemporaryDirectory() as destdir:
            destpath = Path(destdir) / "target"

            await extractor.extract(archive=archive, target=destpath)

            assert destpath.read_text() == "contents"
