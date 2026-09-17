# Development

Pre-requisites:

- [Stack](http://haskellstack.org/)
- [ShellCheck](https://www.shellcheck.net/)
- See [install-dev](bin/install-dev) for more - run it to install automatically.

Run [`./bin/lint`](bin/lint) to check style & types, `./bin/lint --format` to apply
formatting automatically.

## Code style

- Prefer domain types over `Text`. In particular, represent timestamps with `UTCTime`.
- Use modern idioms (e.g., `DerivingVia`, `Generic`, `TypeApplications`).
- Use point-free style where it makes sense.
- Do not use explicit import lists, especially for internal modules, unless
  there are name conflicts. Import commonly conflicted modules qualified (e.g.,
  `import Data.Set qualified as Set`).
- Use the custom `Prelude` (`Mybox.Prelude`) for common imports and utilities.
- Use pattern matching, guards, and monadic control flow (`unlessM`, `whenM`, etc.) extensively.
- Provide type signatures for all top-level functions.
- Use deriving clauses for standard typeclasses (`Eq`, `Show`, `Generic`, etc.), including `DerivingVia` where appropriate.

## Architecture

- Effects are tracked using `effectful`.
- All system operations (filesystem, process, etc.) are performed through the
  `Driver` effect. IO or separate packages are not used to interact with the
  target system.
- Use the `App` type alias to define the effect stack and make dependencies explicit.
- Organize code into highly modular components, each in its own module.
- Emphasize composability and testability via effects and typeclasses.


## Testing

Run `stack test` to execute the project's tests.

Since the project supports multiple OS and distributions, and modifies the
target system, the tests can be run in a Docker container by specifying
`DOCKER_IMAGE` environment variable, e.g. `DOCKER_IMAGE=ubuntu stack test`.

The full test suite is large and slow, avoid running it unless necessary.
To run test in `Mybox.Some.PackageSpec`, use:

```shell
stack test --ta "--match Mybox.Some.Package"
```

## Running locally

After building (`stack build`), in the directory with package definitions, run:

```shell
$(cd ..path/to/mybox; stack path --local-install-root)/bin/mybox
```

# Contributing

## Commits

Commit messages must follow Semantic Release default conventions:
`type(scope): message`.
* Type `chore` for changes that don't need a release, such as tests, CI, and
  developer documentation.
* Type `fix` for patch-level backwards compatible changes where a hypothetical
  revert would also be backwards compatible.
* Type `feat` for new features.
* `BREAKING CHANGE` footer for breaking changes.
