from .base import PackageTestBase


class TestNeovim(PackageTestBase):
    constructor_args = {
        "repo": "neovim/neovim",
        "exclude": "sha256sum",
        "strip": 1,
        "binary": "nvim",
        "app": "nvim",
    }
