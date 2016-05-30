Bound
=====

[![Hackage](https://img.shields.io/hackage/v/bound.svg)](https://hackage.haskell.org/package/bound) [![Build Status](https://secure.travis-ci.org/ekmett/bound.png?branch=master)](http://travis-ci.org/ekmett/bound)

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
import Bound
import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Foldable
import Data.Traversable

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

-- Unfortunately we have to write these instances by hand, for now.
--
-- https://mail.haskell.org/pipermail/libraries/2016-January/026536.html
-- https://github.com/haskell-compat/deriving-compat/issues/3
instance Eq1 Exp where
  liftEq g (V a)     (V b)     = g a b
  liftEq g (a :@ a') (b :@ b') = liftEq g a b && liftEq g a' b'
  liftEq g (Lam a)   (Lam b)   = liftEq g a b
  liftEq _ _         _         = False

instance Show1 Exp where
  liftShowsPrec g _ d (V a) =
    showParen (d >= 11)
      $ showString "V "
      . g 11 a
  liftShowsPrec g h d (a :@ b) =
    showParen (d >= 10)
      $ liftShowsPrec g h 10 a
      . showString " :@ "
      . liftShowsPrec g h 10 b
  liftShowsPrec g h d (Lam a) =
    showParen (d >= 11)
      $ showString "Lam "
      . liftShowsPrec g h 11 a

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

