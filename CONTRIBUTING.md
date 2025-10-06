# Development

Pre-requisites:

* [Stack](http://haskellstack.org/)
* [ShellCheck](https://www.shellcheck.net/)
* See [install-dev](bin/install-dev) for more - run it to install automatically.

Run [`./bin/lint`](bin/lint) to check style & types, `./bin/lint --format` to apply
formatting automatically.

## Testing

Run `stack test` to execute the project's tests.

Since the project supports multiple OS and distributions, and modifies the
target system, the tests can be run in a Docker container by specifying
`DOCKER_IMAGE` environment variable, e.g. `DOCKER_IMAGE=ubuntu stack test`.

## Running locally

After building (`stack build`), in the directory with package definitions, run:

```shell
$(cd ..path/to/mybox; stack path --local-install-root)/bin/mybox
```

## Releasing

Releases are done using Semantic Release, see [build.yml](.github/workflows/build.yml).


# Code style

- Use modern idioms (e.g., `DerivingVia`, `Generic`, `TypeApplications`).
- Use point-free style where it makes sense.
- Prefer wildcard module imports, unless there are name conflicts. Import
  commonly conflicted modules qualified (e.g., `import Data.Set qualified as Set`).
- Use the custom `Prelude` (`Mybox.Prelude`) for common imports and utilities.
- Use pattern matching, guards, and monadic control flow (`unlessM`, `whenM`, etc.) extensively.
- Provide type signatures for all top-level functions.
- Use deriving clauses for standard typeclasses (`Eq`, `Show`, `Generic`, etc.), including `DerivingVia` where appropriate.

# Architecture

- Effects are tracked using `effectful`.
- All system operations (filesystem, process, etc.) are performed through the
  `Driver` effect. IO or separate packages are not used to interact with the
  target system.
- Use the `App` type alias to define the effect stack and make dependencies explicit.
- Organize code into highly modular components, each in its own module.
- Emphasize composability and testability via effects and typeclasses.

# Testing

To run a (Haskell) test in `Mybox.Some.PackageSpec`, use:

```shell
stack test --ta "--match Mybox.Some.Package"
```
