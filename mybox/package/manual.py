import configparser
from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import Optional

from ..utils import Some, transplant_path, unsome
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
            return self.driver.local()

    @abstractmethod
    def binary_path(self, binary: str) -> Path:
        pass

    def install_binary(self, name: str) -> None:
        self.driver.link(
            self.binary_path(name),
            self.local / "bin" / name,
            method="binary_wrapper" if self.binary_wrapper else None,
        )

    @abstractmethod
    def app_path(self, name: str) -> Path:
        pass

    def icon_directory(self) -> Optional[Path]:
        return None

    def icon_name(self, app_path: Path) -> Optional[str]:
        config = self.driver.read_file(app_path)
        app = configparser.ConfigParser()
        app.read_string(config)
        return app["Desktop Entry"].get("Icon")

    def install_app(self, name: str) -> None:
        self.driver.os.switch(
            linux=self.install_app_linux, macos=self.install_app_macos
        )(name)

    def install_app_linux(self, name: str) -> None:
        path = self.app_path(name)
        target = self.local / "share" / "applications" / f"{name}.desktop"
        self.driver.link(path, target)
        icons_source = self.icon_directory()  # pylint:disable=assignment-from-none
        if icons_source:
            icons_target = self.local / "share" / "icons"
            icon = self.icon_name(path)
            if icon:
                icons = self.driver.run_output(
                    "find", str(icons_source), "-name", f"{icon}.*"
                ).splitlines()
                for icon_path in map(Path, icons):
                    target = transplant_path(icons_source, icons_target, icon_path)
                    self.driver.link(icon_path, target)

    def install_app_macos(self, name: str) -> None:
        # FIXME: copy to /Applications and/or ~/Applications; ensure names,
        # etc. are correct
        pass

    @abstractmethod
    def font_path(self, name: str) -> Path:
        pass

    def install_font(self, name: str) -> None:
        font_dir = self.driver.os.switch(
            linux=self.local / "share" / "fonts",
            macos=self.driver.home() / "Library" / "Fonts",
        )
        self.driver.makedirs(font_dir)
        source = self.font_path(name)
        target = font_dir / name
        self.driver.link(source, target)
        if self.driver.executable_exists("fc-cache"):
            self.driver.run("fc-cache", "-f", str(font_dir))

    def install(self) -> None:
        for binary in self.binaries:
            self.install_binary(binary)
        for app in self.apps:
            self.install_app(app)
        for font in self.fonts:
            self.install_font(font)
        super().install()
