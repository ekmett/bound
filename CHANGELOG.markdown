2.0.3 [2021.02.05]
------------------
* Allow the examples to build with `vector-0.12.2` or later.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec) to run the doctests.

2.0.2 [2020.10.01]
------------------
* Allow building with GHC 9.0.

2.0.1
-----
* Add `abstractEither` and `instantiateEither` to `Bound.Scope`, and
  add `abstractEitherName` and `instantiateEitherName` to `Bound.Scope.Name`
* Add `Generic(1)` instances for `Name` and `Scope`
* Support `doctest-0.12`

2
-
* GHC 8.0 and 8.2 support
* Converted from `prelude-extras` to `transformers` + `transformers-compat` for the `Eq1`, `Ord1`, `Show1`, and `Read1` functionality.
* `makeBound` supports `Functor` components
* Add `MFunctor` instance for `Scope`
* Add `NFData` instances for `Name`, `Scope`, and `Var`
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

1.0.7
------
* Added an `-f-template-haskell` option to allow disabling `template-haskell` support. This is an unsupported configuration but may be useful for expert users in sandbox configurations.
* Support `cereal` 0.5

1.0.6
-----
* Compiles warning-free on GHC 7.10

1.0.5
-----
* Widened version bound on `bifunctors`.
* Widened version bound on `profunctors`.

1.0.4
-----
* Widened version bound on `transformers`.

1.0.3
-----
* Added `bitransverseScope`.

1.0.2
-----
* Removed unneccesary constraint on `hoistScope`.

1.0.1
-----
* Added a monomorphic `hoistScope` for `Bound.Scope.Simple`

1.0
---
* Added instances for `Bound` for all of the `mtl` monads.
* Added `Data` and `Typeable` support to both versions of `Scope`
* Added the missing `Applictive` instance to `Bound.Scope.Simple`
* Moved `hoistScope`, `bitraverseScope`, `transverseScope`, and `instantiateVars` here from the `ermine` compiler.

0.9.1.1
-------
* Updated to work with `bifunctors` 4.0

0.9.1
-----
* Updated to work with `comonad` 4.0 and `profunctors` 4.0

0.9
---
* Added the missing instance for `Applicative (Scope b f)`

0.8.1
-----
* SafeHaskell support

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
