-----------------------------------------------------------------------------
-- |
-- Module      :  Bound
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- We represent the target language itself as an ideal monad supplied by the
-- user, and provide a 'Scope' monad transformer for introducing bound
-- variables in user supplied terms. Users supply a 'Monad' and 'Traversable'
-- instance, and we traverse to find free variables, and use the 'Monad' to
-- perform substitution that avoids bound variables.
--
-- An untyped lambda calculus:
--
-- @
-- {-\# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable \#-}
-- import Bound
-- import Control.Applicative
-- import Control.Monad ('Control.Monad.ap')
-- import Prelude.Extras
-- import Data.Foldable
-- import Data.Traverable
-- @
--
-- @
-- infixl 9 :\@
-- data Exp a = V a | Exp a :\@ Exp a | Lam ('Scope' () Exp a)
--   deriving ('Eq','Ord','Show','Read','Functor','Data.Foldable.Foldable','Data.Foldable.Traversable')
-- @
--
-- @
-- instance 'Prelude.Extras.Eq1' Exp   where ('Prelude.Extras.==#')      = ('==')
-- instance 'Prelude.Extras.Ord1' Exp  where 'Prelude.Extras.compare1'   = 'compare'
-- instance 'Prelude.Extras.Show1' Exp where 'Prelude.Extras.showsPrec1' = 'showsPrec'
-- instance 'Prelude.Extras.Read1' Exp where 'Prelude.Extras.readsPrec1' = 'readsPrec'
-- instance 'Control.Applicative.Applicative' Exp where 'Control.Applicative.pure' = V; ('<*>') = 'Control.Monad.ap'
-- @
--
-- @
-- instance 'Monad' Exp where
--   'return' = V
--   V a      '>>=' f = f a
--   (x :\@ y) '>>=' f = (x '>>=' f) :\@ (y >>= f)
--   Lam e    '>>=' f = Lam (e '>>>=' f)
-- @
--
-- @
-- lam :: 'Eq' a => a -> 'Exp' a -> 'Exp' a
-- lam v b = Lam ('abstract1' v b)
-- @
--
-- @
-- whnf :: 'Exp' a -> 'Exp' a
-- whnf (f :\@ a) = case whnf f of
--   Lam b -> whnf ('instantiate1' a b)
--   f'    -> f' :\@ a
-- whnf e = e
-- @
--
-- More exotic combinators for manipulating a 'Scope' can be imported from
-- "Bound.Scope".
--
----------------------------------------------------------------------------
module Bound
  (
  -- * Manipulating user terms
    substitute
  , isClosed
  , closed
  -- * Scopes introduce bound variables
  , Scope(..)
  -- ** Abstraction over bound variables
  , abstract, abstract1
  -- ** Instantiation of bound variables
  , instantiate, instantiate1
  -- * Structures permitting substitution
  , Bound(..)
  , (=<<<)
  -- * Conversion to Traditional de Bruijn
  , Var(..)
  , fromScope
  , toScope
  ) where

import Bound.Var
import Bound.Class
import Bound.Scope
import Bound.Term
