# Mybox

🖥️ This is a box. 📦 And it is mine. 🐱

There are many 🍱 nice things in there. I wouldn't want 🧰 to be without them.

Even if I move 🏠 or work 🏢 I want to be comfortable.

---

Manage the configuration and tools on your workstation without bothering the OS
too much (maybe your favorite one isn't supported by `$WORK` or you have
different ones for different roles).

## Values

0. Earlier points override latter ones, unless there is a huge gain.
1. The system is usable with the software the user specified.
2. Meta requirements (e.g. archivers and package managers) are installed the
   same way user requirements are.
3. Everything is installed and upgraded to the latest version every time.
4. Superuser access is not required.

## Usage

### The first time

Download the right release for your system and make it executable:

```shell
mkdir -p ~/.local/bin
curl -fsSL "https://github.com/koterpillar/mybox/releases/latest/download/mybox-$(uname -s)-$(uname -m)" -o ~/.local/bin/mybox
chmod +x ~/.local/bin/mybox
```

Make sure `~/.local/bin` is in your `PATH`.

### Then

Run `mybox` from the directory with package definitions.

For package definition examples, see
[koterpillar/desktop](https://github.com/koterpillar/desktop/).
