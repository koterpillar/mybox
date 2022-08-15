import os

from .base import PackageTestBase


class TestLinks(PackageTestBase):
    constructor_args = {
        "links": f"{os.path.dirname(__file__)}/test_links_content",
        "dest": "config",
    }

    @property
    def check_installed_command(self) -> list[str]:
        return ["cat", f"{os.environ['MYBOX_HOME']}/config/myfile"]

    check_installed_output = "Linked file"
