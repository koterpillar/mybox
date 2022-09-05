from .base import PackageTestBase


class TestNeovim(PackageTestBase):
    constructor_args = {
        "repo": "neovim/neovim",
        "exclude": "sha256sum",
        "strip": 1,
        "binary": "nvim",
        "app": "nvim",
    }

    check_installed_command = ["nvim", "--version"]

    check_installed_output = "NVIM"


class TestExa(PackageTestBase):
    constructor_args = {
        "repo": "ogham/exa",
        "binary": "exa",
    }

    check_installed_command = ["exa", "--version"]

    check_installed_output = "exa - list files"


class TestAmmonite(PackageTestBase):
    constructor_args = {
        "repo": "com-lihaoyi/Ammonite",
        "regex": r"^2\.13-[0-9]+\.[0-9]+\.[0-9]+-bootstrap$",
        "raw": "amm",
        "raw_executable": True,
        "binary": "amm",
    }

    check_installed_command = ["amm", "--version"]

    check_installed_output = "Ammonite REPL"
