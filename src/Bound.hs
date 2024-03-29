{-# LANGUAGE CPP             #-}

-----------------------------------------------------------------------------
-- |
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
-- {-\# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell \#-}
-- import Bound
-- import Control.Applicative
-- import Control.Monad ('Control.Monad.ap')
-- import Data.Functor.Classes
-- import Data.Foldable
-- import Data.Traversable
-- -- This is from deriving-compat package
-- import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
-- @
--
-- @
-- infixl 9 :\@
-- data Exp a = V a | Exp a :\@ Exp a | Lam ('Scope' () Exp a)
--   deriving ('Functor','Data.Foldable.Foldable','Data.Foldable.Traversable')
-- @
--
-- @
-- instance 'Control.Applicative.Applicative' Exp where 'Control.Applicative.pure' = V; ('<*>') = 'Control.Monad.ap'
-- instance 'Monad' Exp where
--   'return' = V
--   V a      '>>=' f = f a
--   (x :\@ y) '>>=' f = (x '>>=' f) :\@ (y '>>=' f)
--   Lam e    '>>=' f = Lam (e '>>>=' f)
-- @
--
-- @
-- concat <$> sequence
--   [ deriveEq1   ''Exp
--   , deriveOrd1  ''Exp
--   , deriveRead1 ''Exp
--   , deriveShow1 ''Exp
--
--   , [d| instance 'Eq' a   => 'Eq'   (Exp a) where (==) = eq1
--         instance 'Ord' a  => 'Ord'  (Exp a) where compare = compare1
--         instance 'Show' a => 'Show' (Exp a) where showsPrec = showsPrec1
--         instance 'Read' a => 'Read' (Exp a) where readsPrec = readsPrec1
--       |]
--   ]
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
-- You can also retain names in your bound variables by using 'Bound.Name.Name'
-- and the related combinators from "Bound.Name". They are not re-exported
-- from this module by default.
--
-- The approach used in this package was first elaborated upon by Richard Bird
-- and Ross Patterson
-- in \"de Bruijn notation as a nested data type\", available from
-- <http://www.cs.uwyo.edu/~jlc/courses/5000_fall_08/debruijn_as_nested_datatype.pdf>
--
-- However, the combinators they used required higher rank types. Here we
-- demonstrate that the higher rank @gfold@ combinator they used isn't necessary
-- to build the monad and use a monad transformer to encapsulate the novel
-- recursion pattern in their generalized de Bruijn representation. It is named
-- 'Scope' to match up with the terminology and usage pattern from Conor McBride
-- and James McKinna's \"I am not a number: I am a free variable\", available
-- from <http://www.cs.ru.nl/~james/RESEARCH/haskell2004.pdf>, but since
-- the set of variables is visible in the type, we can provide stronger type
-- safety guarantees.
--
-- There are longer examples in the @examples/@ folder:
--
-- <https://github.com/ekmett/bound/tree/master/examples>
--
-- (1) /Simple.hs/ provides an untyped lambda calculus with recursive let
--   bindings and includes an evaluator for the untyped lambda calculus and a
--   longer example taken from Lennart Augustsson's "λ-calculus cooked four
--   ways" available from <http://foswiki.cs.uu.nl/foswiki/pub/USCS/InterestingPapers/AugustsonLambdaCalculus.pdf>
--
-- 2. /Derived.hs/ shows how much of the API can be automated with
--    DeriveTraversable and adds combinators for building binders that support
--    pattern matching.
--
-- 3. /Overkill.hs/ provides very strongly typed pattern matching many modern
--   language extensions, including polymorphic kinds to ensure type safety.
--   In general, the approach taken by Derived seems to deliver a better power
--   to weight ratio.
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
#ifdef MIN_VERSION_template_haskell
  -- * Deriving instances
  , makeBound
#endif
  ) where

import Bound.Var
import Bound.Class
import Bound.Scope
import Bound.Term
#ifdef MIN_VERSION_template_haskell
import Bound.TH
#endif
