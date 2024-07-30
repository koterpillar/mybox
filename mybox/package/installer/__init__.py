from ...driver import Driver
from ...utils import async_cached_lock
from .apt import Apt
from .base import Installer
from .brew import Brew
from .dnf import DNF


async def linux_installer(driver: Driver) -> Installer:
    if await driver.executable_exists("dnf"):
        return DNF(driver)
    elif await driver.executable_exists("apt"):
        return Apt(driver)
    else:
        raise NotImplementedError("Cannot find a package manager.")


async def macos_installer(driver: Driver) -> Installer:
    return Brew(driver)


@async_cached_lock
async def make_installer(driver: Driver) -> Installer:
    os = await driver.os()
    installer_fn = os.switch(linux=linux_installer, macos=macos_installer)
    return await installer_fn(driver)
