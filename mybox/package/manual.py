import configparser
import shutil
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Optional

from ..fs import transplant_path
from ..utils import Some, unsome, with_os
from .manual_version import ManualVersion
from .root import Root


class ManualPackage(Root, ManualVersion, metaclass=ABCMeta):
    binaries: list[str]
    apps: list[str]
    fonts: list[str]

    def __init__(
        self,
        *,
        binary: Some[str] = None,
        binary_wrapper: bool = False,
        app: Some[str] = None,
        font: Some[str] = None,
        **kwargs,
    ) -> None:
        super().__init__(**kwargs)
        self.binaries = unsome(binary)
        self.binary_wrapper = binary_wrapper
        self.apps = unsome(app)
        self.fonts = unsome(font)

    @property
    def local(self) -> Path:
        if self.root:
            return Path("/usr/local")
        else:
            return self.fs.local()

    @abstractmethod
    def binary_path(self, binary: str) -> Path:
        pass

    def install_binary(self, name: str) -> None:
        self.fs.link(
            self.binary_path(name),
            self.local / "bin" / name,
            method="binary_wrapper" if self.binary_wrapper else None,
        )

    @abstractmethod
    def app_path(self, name: str) -> Path:
        pass

    def icon_directory(self) -> Optional[Path]:
        return None

    def install_app(self, name: str) -> None:
        with_os(linux=self.install_app_linux, macos=self.install_app_macos)(name)

    def icon_name(self, app_path: Path) -> Optional[str]:
        config = self.fs.read_file(app_path)
        app = configparser.ConfigParser()
        app.read_string(config)
        return app["Desktop Entry"].get("Icon")

    def install_app_linux(self, name: str) -> None:
        path = self.app_path(name)
        target = self.local / "share" / "applications" / f"{name}.desktop"
        self.fs.link(path, target)
        icons_source = self.icon_directory()  # pylint:disable=assignment-from-none
        if icons_source:
            icons_target = self.local / "share" / "icons"
            icon = self.icon_name(path)
            if icon:
                for icon_path in icons_source.rglob(f"{icon}.*"):
                    target = transplant_path(icons_source, icons_target, icon_path)
                    self.fs.link(icon_path, target)

    def install_app_macos(self, name: str) -> None:
        # FIXME: copy to /Applications and/or ~/Applications; ensure names,
        # etc. are correct
        pass

    @abstractmethod
    def font_path(self, name: str) -> Path:
        pass

    def install_font(self, name: str) -> None:
        font_dir = with_os(
            linux=self.local / "share" / "fonts",
            macos=self.fs.home() / "Library" / "Fonts",
        )
        self.fs.makedirs(font_dir)
        source = self.font_path(name)
        target = font_dir / name
        self.fs.link(source, target)
        if shutil.which("fc-cache"):
            self.fs.run("fc-cache", "-f", str(font_dir))

    def install(self) -> None:
        for binary in self.binaries:
            self.install_binary(binary)
        for app in self.apps:
            self.install_app(app)
        for font in self.fonts:
            self.install_font(font)
        super().install()
