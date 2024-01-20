# Mybox

🖥️ This is a box. 📦 And it is mine. 🐱

There are many 🍱 nice things in there. I wouldn't want 🧰 to be without them.

Even if I move 🏠 or work 🏢 I want to be comfortable.

---

Manage the configuration and tools on your workstation without bothering the OS
too much (maybe your favorite one isn't supported by `$WORK` or you have
different ones for different roles).

## Usage

* Run the [bootstrap](bootstrap) script:

  ```shell
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/koterpillar/mybox/main/bootstrap)"
  ```

* Run `mybox` from the directory with package definitions.

  For package definition examples, see
  [koterpillar/desktop](https://github.com/koterpillar/desktop/).

* To install optional components, add their names as arguments, e.g.
  `mybox development`.

## Development

Pre-requisites (see [install-dev](install-dev) for ways to install):

* [Poetry](https://python-poetry.org/)
* [ShellCheck](https://www.shellcheck.net/)

Run [`./lint`](lint) to check style & types, `./lint --format` to apply
formatting automatically.

Run [`./test-script`](test-script) to execute the project's tests.

### Running locally

* Run `poetry install`.
* Run `poetry shell`.
* In the launched shell, go to the directory with package definitions.
* Run `mybox` with the desired arguments.

### Releasing

Releases are done using Semantic Release, see [build.yml](.github/workflows/build.yml).
