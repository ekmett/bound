Bound
=====

[![Hackage](https://img.shields.io/hackage/v/bound.svg)](https://hackage.haskell.org/package/bound) [![Build Status](https://github.com/ekmett/bound/workflows/Haskell-CI/badge.svg)](https://github.com/ekmett/bound/actions?query=workflow%3AHaskell-CI)

Goals
-----

This library provides convenient combinators for working with "locally-nameless" terms. These can be useful
when writing a type checker, evaluator, parser, or pretty printer for terms that contain binders like forall
or lambda, as they ease the task of avoiding variable capture and testing for alpha-equivalence.

See [the documentation](http://hackage.haskell.org/package/bound) on hackage for more information, but here is an example:

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

import Bound
import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Foldable
import Data.Traversable
import Data.Eq.Deriving (deriveEq1)      -- these two are from the
import Text.Show.Deriving (deriveShow1)  -- deriving-compat package

infixl 9 :@
data Exp a = V a | Exp a :@ Exp a | Lam (Scope () Exp a)
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance Applicative Exp where pure = V; (<*>) = ap

instance Monad Exp where
  return = V
  V a      >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)

lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

whnf :: Exp a -> Exp a
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1 a b)
  f'    -> f' :@ a
whnf e = e

deriveEq1 ''Exp
deriveShow1 ''Exp

main :: IO ()
main = do
  let term = lam 'x' (V 'x') :@ V 'y'
  print term         -- Lam (Scope (V (B ()))) :@ V 'y'
  print $ whnf term  -- V 'y'
```

   There are longer examples in the [examples/ folder](https://github.com/ekmett/bound/tree/master/examples).

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

