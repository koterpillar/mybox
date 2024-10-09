import re
import shlex
from pathlib import Path
from textwrap import dedent
from typing import Optional

from pydantic import Field

from ..tracker import Tracker
from ..utils import allow_singular
from .base import Package


class Daemon(Package):
    daemon: list[str] = Field(default_factory=list)
    daemon_val = allow_singular("daemon")

    @property
    def daemon_cmd(self) -> str:
        return shlex.join(self.daemon)

    def derive_name(self) -> str:
        return f"daemon-{self.daemon_cmd}"

    async def get_remote_version(self) -> str:
        return self.daemon_cmd

    async def local_version(self) -> Optional[str]:
        if await self.driver.is_file(await self.service_file_path()):
            return self.daemon_cmd
        return None

    @property
    def daemon_name(self) -> str:
        return "com.koterpillar.mybox." + re.sub(
            r"\W", lambda c: "" if c.group(0) in {'"', "'"} else "_", self.daemon_cmd
        )

    @property
    def daemon_description(self) -> str:
        return f"Mybox: {self.name_ or self.daemon_cmd}"

    async def service_file(self) -> str:
        return (await self.driver.os()).switch(
            linux=dedent(
                f"""\
                [Unit]
                Description={self.daemon_description}

                [Service]
                Restart=always
                ExecStart=/bin/sh -c {shlex.quote(self.daemon_cmd)}

                [Install]
                WantedBy=default.target
                """
            ),
            macos=dedent(
                f"""\
                <?xml version="1.0" encoding="UTF-8"?>
                <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
                <plist version="1.0">
                <dict>
                    <key>Label</key>
                    <string>{self.daemon_description}</string>
                    <key>RunAtLoad</key>
                    <true/>
                    <key>KeepAlive</key>
                    <true/>
                    <key>ProgramArguments</key>
                    <array>
                    <string>/usr/bin/env</string>
                    <string>-SPATH=${{HOME}}/.local/bin:${{PATH}}</string>
                    <string>/bin/sh</string>
                    <string>-c</string>
                    <string>{self.daemon_cmd}</string>
                    </array>
                </dict>
                </plist>"""
            ),
        )

    async def service_file_path(self) -> Path:
        home = await self.driver.home()
        local = await self.driver.local()
        return (await self.driver.os()).switch_(
            linux=lambda linux: local
            / "share"
            / "systemd"
            / "user"
            / f"{self.daemon_name}.service",
            macos=lambda: home
            / "Library"
            / "LaunchAgents"
            / f"{self.daemon_name}.plist",
        )

    async def install(self, *, tracker: Tracker) -> None:
        service_file_path = await self.service_file_path()
        await self.driver.write_file(service_file_path, await self.service_file())
        tracker.track(service_file_path)

        await (await self.driver.os()).switch(
            linux=self.register_linux, macos=self.register_macos
        )()

    async def register_linux(self) -> None:
        await self.driver.run("systemctl", "--user", "daemon-reload")
        await self.driver.run("systemctl", "--user", "start", self.daemon_name)

    async def register_macos(self) -> None:
        await self.driver.run("launchctl", "load", await self.service_file_path())
