0.8
---
* Added `Serial`, `Binary` and `Serialize` instances for `Scope`.

0.7
---
* Added `Hashable`, `Hashable1` and `Hashable2` instances where appropriate for `Name`, `Var` and `Scope`.

0.6.1
-----
* More aggressive inlining
* Added `unvar`, `_B`, `_F` to `Bound.Var`.
* Added `_Name` to `Bound.Name`.

0.6
---
* Support for `prelude-extras` 0.3

0.5.1
-----
* Removed my personal inter-package dependency upper bounds
* Updated doctest suite to use exact versions.

0.5
---
* Created a `doctest`-based test suite
* Added many examples
* 100% haddock coverage
* Added the `Name` `Comonad`, to help retain names for bound variables.
* Bumped dependencies
