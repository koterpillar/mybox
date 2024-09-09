import subprocess
from abc import ABC, abstractmethod
from contextlib import asynccontextmanager, nullcontext
from dataclasses import dataclass
from os import environ
from pathlib import Path
from typing import (
    Any,
    AsyncContextManager,
    AsyncIterator,
    Callable,
    Iterable,
    Literal,
    Optional,
    cast,
)

from trio import run_process

from .parallel import parallel_map_pause
from .utils import RunArg, Some, T, async_cached, intercalate, unsome


class OS(ABC):
    @abstractmethod
    def switch_(self, *, linux: Callable[["Linux"], T], macos: Callable[[], T]) -> T:
        pass

    def switch(self, *, linux: T, macos: T) -> T:
        return self.switch_(linux=lambda _: linux, macos=lambda: macos)


@dataclass
class Linux(OS):
    distribution: str

    def switch_(self, *, linux: Callable[["Linux"], T], macos: Callable[[], T]) -> T:
        return linux(self)

    RELEASE_FILE = "/etc/os-release"

    @classmethod
    async def get_distribution(cls, driver: "Driver") -> str:
        release_file = await driver.read_file(Path(cls.RELEASE_FILE))
        for line in release_file.splitlines():
            k, v = line.split("=", 1)
            if k == "ID":
                return v

        raise ValueError(f"Cannot find distribution ID in {cls.RELEASE_FILE}.")


@dataclass
class MacOS(OS):
    def switch_(self, *, linux: Callable[["Linux"], T], macos: Callable[[], T]) -> T:
        return macos()


Architecture = Literal["x86_64", "aarch64"]


@dataclass
class RunResult:
    ok: bool


@dataclass
class RunResultOutput(RunResult):
    ok: bool
    output: str


class Driver(ABC):
    def __init__(self, *, root: bool = False) -> None:
        self.root = root
        super().__init__()

    def deconstruct(self) -> dict:
        return {"root": self.root}

    def with_root(self, /, root: bool) -> "Driver":
        if self.root == root:
            return self
        kwargs = self.deconstruct() | {"root": root}
        return type(self)(**kwargs)

    @abstractmethod
    async def run_(
        self,
        *args: RunArg,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        show_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        pass

    async def run(
        self,
        *args: RunArg,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        show_output: bool = False,
        silent: bool = False,
    ) -> None:
        await self.run_(*args, input=input, show_output=show_output, silent=silent)

    async def run_ok(self, *args: RunArg) -> bool:
        result = await self.run_(*args, check=False)
        return result.ok

    async def run_output(self, *args: RunArg, silent: bool = False) -> str:
        result = await self.run_(*args, capture_output=True, silent=silent)
        return cast(RunResultOutput, result).output

    async def run_output_(self, *args: RunArg) -> RunResultOutput:
        result = await self.run_(*args, capture_output=True, check=False, silent=True)
        return cast(RunResultOutput, result)

    async def executable_exists(self, executable: str) -> bool:
        return await self.run_ok("sh", "-c", f"command -v {executable}")

    async def find_executable(self, *executables: str) -> str:
        for candidate in executables:
            if await self.executable_exists(candidate):
                return candidate
        raise Exception(f"None of {', '.join(executables)} found in PATH.")

    async def is_file(self, path: Path) -> bool:
        return await self.run_ok("test", "-f", path)

    async def is_executable(self, path: Path) -> bool:
        return await self.run_ok("test", "-x", path)

    async def is_dir(self, path: Path) -> bool:
        return await self.run_ok("test", "-d", path)

    async def username(self) -> str:
        if self.root:
            return "root"
        return await self.run_output("whoami")

    @async_cached
    async def home(self) -> Path:
        path = "~root" if self.root else "~"
        result = await self.with_root(False).run_output("sh", "-c", f"eval echo {path}")
        return Path(result)

    async def local(self) -> Path:
        if self.root:
            return Path("/usr/local")
        return await self.home() / ".local"

    async def find(
        self,
        path: Path,
        *,
        name: Some[str] = None,
        file_type: Some[str] = None,
        mindepth: Optional[int] = None,
        maxdepth: Optional[int] = None,
    ) -> list[Path]:
        args: list[RunArg] = ["find", path]

        def add_some_arg(arg: str, values: list[str]) -> None:
            if values:
                if len(values) > 1:
                    args.append("(")
                args.extend(intercalate("-o", ([arg, v] for v in values)))
                if len(values) > 1:
                    args.append(")")

        def add_optional_arg(arg: str, value: Optional[Any]) -> None:
            if value is not None:
                args.extend([arg, str(value)])

        add_optional_arg("-mindepth", mindepth)
        add_optional_arg("-maxdepth", maxdepth)
        add_some_arg("-name", unsome(name))
        add_some_arg("-type", unsome(file_type))

        args.append("-print0")

        output = await self.run_output(*args)
        return [Path(result) for result in output.split("\0") if result]

    @asynccontextmanager
    async def tempfile(
        self, kind: Optional[Literal["directory"]] = None
    ) -> AsyncIterator[Path]:
        args = []
        if kind == "directory":
            args.append("-d")

        path = Path(await self.run_output("mktemp", *args))
        try:
            yield path
        finally:
            await self.rm(path)

    async def makedirs(self, path: Path) -> None:
        await self.run("mkdir", "-p", path)

    async def copy(self, source: Path, target: Path) -> None:
        await self.makedirs(target.parent)
        await self.rm(target)
        await self.run("cp", "-R", "-f", source, target)

    async def rm(self, path: Path) -> None:
        await self.run("rm", "-r", "-f", path)

    async def make_executable(self, path: Path) -> None:
        await self.run("chmod", "+x", path)

    async def read_file(self, path: Path) -> str:
        return await self.run_output("cat", path)

    async def write_file(self, path: Path, content: str) -> None:
        await self.makedirs(path.parent)
        await self.rm(path)
        await self.run("cp", "/dev/stdin", path, input=content.encode())

    async def link(
        self,
        source: Path,
        target: Path,
    ) -> None:
        await self.makedirs(target.parent)
        await self.rm(target)
        await self.run("ln", "-s", "-f", source, target)

    @async_cached
    async def os(self) -> OS:
        driver = self.with_root(False)
        os_type = await driver.run_output("uname")
        if os_type == "Linux":
            distribution = await Linux.get_distribution(driver)
            return Linux(distribution=distribution)
        elif os_type == "Darwin":
            return MacOS()
        else:
            raise ValueError(f"Unsupported OS type {os_type}.")  # pragma: no cover

    @async_cached
    async def architecture(self) -> Architecture:
        result = await self.with_root(False).run_output("uname", "-m")
        if result == "x86_64":
            return "x86_64"
        elif result in {"arm64", "aarch64"}:
            # No ARM runners on CI
            return "aarch64"  # pragma: no cover
        else:
            raise ValueError(f"Unsupported architecture {result}.")  # pragma: no cover


class SubprocessDriver(Driver, ABC):
    def prepare_command(self, args: Iterable[RunArg]) -> list[RunArg]:
        return list(args)

    async def run_(
        self,
        *args: RunArg,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        show_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        if capture_output and show_output:
            raise ValueError(
                "Cannot use capture_output and show_output at the same time."
            )  # pragma: no cover

        command = self.prepare_command(args)

        if not check or silent:
            stderr = subprocess.DEVNULL
        else:
            stderr = None

        # https://github.com/python/mypy/issues/5512
        cm: AsyncContextManager
        if show_output:
            cm = parallel_map_pause()
        else:
            cm = nullcontext()
        async with cm:
            result = await run_process(
                command,
                check=check,
                stdin=input,
                capture_stdout=not show_output,
                stderr=stderr,
                **self.run_args(),
            )

        ok = result.returncode == 0
        if capture_output:
            output = result.stdout.decode().strip()
            return RunResultOutput(ok=ok, output=output)
        else:
            return RunResult(ok=ok)

    def run_args(self) -> dict[str, Any]:
        return {}


class LocalDriver(SubprocessDriver):
    def prepare_command(self, args: Iterable[RunArg]) -> list[RunArg]:
        if self.root:
            return super().prepare_command(["sudo", *args])
        else:
            return super().prepare_command(args)

    def run_args(self) -> dict[str, Any]:
        result = super().run_args()
        if "VIRTUAL_ENV" in environ:
            # mybox is running in a virtual environment (for testing or
            # development). Remove virtual environment from PATH so that any
            # pip and pipx commands run in the user environment (they will
            # fail otherwise).
            new_environment = environ.copy()
            virtual_env = new_environment.pop("VIRTUAL_ENV")
            new_environment["PATH"] = ":".join(
                segment
                for segment in new_environment["PATH"].split(":")
                if not segment.startswith(virtual_env)
            )
            result["env"] = new_environment
        return result

    async def run_(
        self,
        *args: RunArg,
        check: bool = True,
        input: Optional[bytes] = None,  # pylint:disable=redefined-builtin
        capture_output: bool = False,
        show_output: bool = False,
        silent: bool = False,
    ) -> RunResult:
        if args and args[0] == "sudo":
            # If the sudo prompt is needed, authenticate the user first while
            # showing the prompt.
            await super().run("sudo", "-v", show_output=True)
        return await super().run_(
            *args,
            check=check,
            input=input,
            capture_output=capture_output,
            show_output=show_output,
            silent=silent,
        )
