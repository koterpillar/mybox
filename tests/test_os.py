from typing import Optional

import pytest

from mybox.driver import Driver, Linux, RunResult, RunResultOutput
from mybox.utils import RunArg


class FileReadingDriver(Driver):
    """A dummy driver that only implements file reading functionality."""

    def __init__(self, file_contents: dict[str, str], **kwargs) -> None:
        super().__init__(**kwargs)
        self.file_contents = file_contents

    def deconstruct(self) -> dict:  # pragma: no cover
        return super().deconstruct() | {"file_contents": self.file_contents}

    async def run_(
        self,
        *args: RunArg,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint: disable=redefined-builtin
        capture_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        args_list = [str(arg) for arg in args]

        # Only handle reading files using cat
        if args_list[0] == "cat" and len(args_list) == 2:
            file_path = args_list[1]
            if file_path in self.file_contents:
                return RunResultOutput(ok=True, output=self.file_contents[file_path])
            else:
                return RunResultOutput(ok=False, output="")  # pragma: no cover

        return RunResultOutput(ok=False, output="")  # pragma: no cover


class TestLinux:
    @pytest.mark.trio
    async def test_plain_id(self):
        driver = FileReadingDriver(
            {"/etc/os-release": 'ID=ubuntu\nVERSION_ID="20.04"\nNAME="Ubuntu"'}
        )

        distribution = await Linux.get_distribution(driver)
        assert distribution == "ubuntu"

    @pytest.mark.trio
    async def test_quoted_id(self):
        """Test parsing distribution ID that is quoted."""
        driver = FileReadingDriver(
            {"/etc/os-release": 'ID="debian"\nVERSION_ID="11"\nNAME="Debian GNU/Linux"'}
        )

        distribution = await Linux.get_distribution(driver)
        assert distribution == "debian"

    @pytest.mark.trio
    async def test_multiline_with_id_in_middle(self):
        """Test parsing when ID line is in the middle of the file."""
        driver = FileReadingDriver(
            {
                "/etc/os-release": """NAME="Alpine Linux"
VERSION_ID=3.17.0
PRETTY_NAME="Alpine Linux v3.17"
HOME_URL="https://alpinelinux.org/"
BUG_REPORT_URL="https://bugs.alpinelinux.org/"
ID=alpine
VERSION_CODENAME=""
ANSI_COLOR="1;34\""""
            }
        )

        distribution = await Linux.get_distribution(driver)
        assert distribution == "alpine"

    @pytest.mark.trio
    async def test_missing_id_raises_error(self):
        """Test that missing ID in os-release file raises ValueError."""
        driver = FileReadingDriver(
            {
                "/etc/os-release": 'NAME="Some Linux"\nVERSION="1.0"\nPRETTY_NAME="Some Linux 1.0"'
            }
        )

        with pytest.raises(
            ValueError, match="Cannot find distribution ID in /etc/os-release"
        ):
            await Linux.get_distribution(driver)

    @pytest.mark.trio
    async def test_empty_file_raises_error(self):
        """Test that empty os-release file raises ValueError."""
        driver = FileReadingDriver({"/etc/os-release": ""})

        with pytest.raises(
            ValueError, match="Cannot find distribution ID in /etc/os-release"
        ):
            await Linux.get_distribution(driver)
