{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}

# if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DeriveGeneric #-}
# endif

#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Bound.Name
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The problem with locally nameless approaches is that original names are
-- often useful for error reporting, or to allow for the user in an interactive
-- theorem prover to convey some hint about the domain. A @'Name' n b@ is a value
-- @b@ supplemented with a (discardable) name that may be useful for error
-- reporting purposes. In particular, this name does not participate in
-- comparisons for equality.
--
-- This module is /not/ exported from "Bound" by default. You need to explicitly
-- import it, due to the fact that 'Name' is a pretty common term in other
-- people's code.
----------------------------------------------------------------------------
module Bound.Name
  ( Name(..)
  , name
  , abstractName
  , abstract1Name
  , instantiateName
  , instantiate1Name
  ) where

import Bound.Scope
import Bound.Var
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
#ifdef __GLASGOW_HASKELL__
import Data.Data
# if __GLASGOW_HASKELL__ >= 704
import GHC.Generics
# endif
#endif
import Control.Applicative
import Control.Comonad
import Control.Monad (liftM)
import Prelude.Extras

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

-- |
-- We track the choice of 'Name' @n@ as a forgettable property that does /not/ affect
-- the result of ('==') or 'compare'.
--
-- To compare names rather than values, use @('Data.Function.on' 'compare' 'name')@ instead.
data Name n b = Name n b deriving
  ( Show
  , Read
#ifdef __GLASGOW_HASKELL__
  , Typeable
  , Data
# if __GLASGOW_HASKELL__ >= 704
  , Generic
# endif
#endif
  )

-- | Extract the 'name'.
name :: Name n b -> n
name (Name n _) = n

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq b => Eq (Name n b) where
  Name _ a == Name _ b = a == b

instance Ord b => Ord (Name n b) where
  Name _ a `compare` Name _ b = compare a b

instance Functor (Name n) where
  fmap f (Name n a) = Name n (f a)

instance Foldable (Name n) where
  foldMap f (Name _ a) = f a

instance Traversable (Name n) where
  traverse f (Name n a) = Name n <$> f a

instance Bifunctor Name where
  bimap f g (Name n a) = Name (f n) (g a)

instance Bifoldable Name where
  bifoldMap f g (Name n a) = f n `mappend` g a

instance Bitraversable Name where
  bitraverse f g (Name n a) = Name <$> f n <*> g a

instance Comonad (Name n) where
  extract (Name _ b) = b
  extend f w@(Name n _) = Name n (f w)

instance Eq1   (Name b) where (==#)      = (==)
instance Ord1  (Name b) where compare1   = compare
instance Show b => Show1 (Name b) where showsPrec1 = showsPrec
instance Read b => Read1 (Name b) where readsPrec1 = readsPrec

-- these are slightly too restrictive, but still safe
instance Eq2 Name   where (==##)     = (==)
instance Ord2 Name  where compare2   = compare
instance Show2 Name where showsPrec2 = showsPrec
instance Read2 Name where readsPrec2  = readsPrec

-------------------------------------------------------------------------------
-- Abstraction
-------------------------------------------------------------------------------

-- | Abstraction, capturing named bound variables.
abstractName :: Monad f => (a -> Maybe b) -> f a -> Scope (Name a b) f a
abstractName f t = Scope (liftM k t) where
  k a = case f a of
    Just b  -> B (Name a b)
    Nothing -> F (return a)
{-# INLINE abstractName #-}

-- | Abstract over a single variable
abstract1Name :: (Monad f, Eq a) => a -> f a -> Scope (Name a ()) f a
abstract1Name a = abstractName (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1Name #-}

-------------------------------------------------------------------------------
-- Instantiation
-------------------------------------------------------------------------------

-- | Enter a scope, instantiating all bound variables, but discarding (comonadic)
-- meta data, like its name
instantiateName :: (Monad f, Comonad n) => (b -> f a) -> Scope (n b) f a -> f a
instantiateName k e = unscope e >>= \v -> case v of
  B b -> k (extract b)
  F a -> a
{-# INLINE instantiateName #-}


-- | Enter a 'Scope' that binds one (named) variable, instantiating it.
--
-- @'instantiate1Name' = 'instantiate1'@
instantiate1Name :: Monad f => f a -> Scope n f a -> f a
instantiate1Name = instantiate1
{-# INLINE instantiate1Name #-}
