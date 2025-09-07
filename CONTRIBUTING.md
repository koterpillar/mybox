# Code style

- Use modern idioms (e.g., `DerivingVia`, `Generic`, `TypeApplications`).
- Use pointfree style where it makes sense.
- Prefer wildcard module imports, unless there are name conflicts. Import
  commonly conflicted modules qualified (e.g., `import Data.Set qualified as Set`).
- Use the custom `Prelude` (`Mybox.Prelude`) for common imports and utilities.
- Use pattern matching, guards, and monadic control flow (`unlessM`, `whenM`, etc.) extensively.
- Provide type signatures for all top-level functions.
- Use deriving clauses for standard typeclasses (`Eq`, `Show`, `Generic`, etc.), including `DerivingVia` where appropriate.
- Use consistent naming: lowerCamelCase for functions, UpperCamelCase for types.

# Architecture

- All system operations (filesystem, process, etc.) are performed through the
  `Driver` effect. IO or separate packages are not used to interact with the
  target system.
- Use the `App` type alias to define the effect stack and make dependencies explicit.
- Organize code into highly modular components, each in its own module.
- Emphasize composability and testability via effects and typeclasses.
