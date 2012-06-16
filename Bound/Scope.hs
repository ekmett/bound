-----------------------------------------------------------------------------
-- |
-- Module      :  Bound.Scope
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Bound.Scope
  ( Scope(..)
  -- * Abstraction
  , abstract, abstract1
  -- * Instantiation
  , instantiate, instantiate1
  -- * Substitution
  , splat
  -- * Quotienting
  , fromScope
  , toScope
  ) where

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Prelude.Extras
import Bound.Class
import Bound.Var

-- | @Scope b f a@ is a an @f@ expression with bound variables in @b@, and free variables in @a@
--
-- This stores bound variables as their generalized de Bruijn representation,
-- in that the succ's for variable ids are allowed to occur anywhere within the tree
-- permitting /O(1)/ weakening and allowing more sharing opportunities. 
-- Here the deBruijn 0 is represented by the 'B' constructor of 'Var', while the 
-- de Bruijn 'succ' (which may be applied to an entire tree!) is handled by 'F'.
--
-- NB: equality and comparison quotient out the distinct 'F' placements allowed by 
-- the choice of a generalized de Bruijn representation and return the same result as a traditional de Bruijn
-- representation would.

newtype Scope b f a = Scope { unscope :: f (Var b (f a)) }

instance Functor f => Functor (Scope b f) where
  fmap f (Scope a) = Scope (fmap (fmap (fmap f)) a)

-- | @toList@ is provides a list (with duplicates) of the free variables
instance Foldable f => Foldable (Scope b f) where
  foldMap f (Scope a) = foldMap (foldMap (foldMap f)) a

instance Traversable f => Traversable (Scope b f) where
  traverse f (Scope a) = Scope <$> traverse (traverse (traverse f)) a

-- | The monad permits substitution on free variables, while preserving bound variables
instance Monad f => Monad (Scope b f) where
  return a = Scope (return (F (return a)))
  Scope e >>= f = Scope $ e >>= \v -> case v of
    B b -> return (B b)
    F ea -> ea >>= unscope . f

instance MonadTrans (Scope b) where
  lift m = Scope (return (F m))

instance (Monad f, Eq b, Eq1 f, Eq a) => Eq  (Scope b f a) where (==) = (==#)
instance (Monad f, Eq b, Eq1 f)       => Eq1 (Scope b f)   where
  a ==# b = liftM Lift2 (fromScope a) ==# liftM Lift2 (fromScope b)
  -- a ==# b = mangleScope a ==# mangleScope b

instance (Monad f, Ord b, Ord1 f, Ord a) => Ord  (Scope b f a) where compare = compare1
instance (Monad f, Ord b, Ord1 f)        => Ord1 (Scope b f) where
  compare1 a b = liftM Lift2 (fromScope a) `compare1` liftM Lift2 (fromScope b)
  -- compare1 a b = compare1 (mangleScope a) (mangleScope b)

mangleScope :: Functor f => Scope b f a -> f (Lift2 Var b (Lift1 f a))
mangleScope (Scope a) = fmap (Lift2 . fmap Lift1) a
{-# INLINE mangleScope #-}

unmangleScope :: Functor f => f (Lift2 Var b (Lift1 f a)) -> Scope b f a
unmangleScope a = Scope (fmap (fmap lower1 . lower2) a)
{-# INLINE unmangleScope #-}


instance (Functor f, Show b, Show1 f, Show a) => Show  (Scope b f a) where showsPrec = showsPrec1
instance (Functor f, Show b, Show1 f)         => Show1 (Scope b f)   where
  showsPrec1 d a = showParen (d > 10) $ showString "Scope " . showsPrec1 11 (mangleScope a)

instance (Functor f, Read b, Read1 f, Read a) => Read  (Scope b f a) where readsPrec = readsPrec1
instance (Functor f, Read b, Read1 f)         => Read1 (Scope b f) where
  readPrec1 = liftM unmangleScope readPrec1

instance Bound (Scope b) where
  m >>>= f = m >>= lift . f

-- | Capture some free variables in an expression to yield a Scope with bound variables
abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract f e = Scope (liftM k e) where
  k y = case f y of
    Just z  -> B z
    Nothing -> F (return y)
{-# INLINE abstract #-}

-- | Abstract over a single variable
abstract1 :: (Monad f, Eq a) => a -> f a -> Scope () f a
abstract1 a = abstract (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1 #-}

-- | Enter a scope, instantiating all bound variables
instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
instantiate k e = unscope e >>= \v -> case v of
  B b -> k b
  F a -> a
{-# INLINE instantiate #-}

-- | Enter a scope with one bound variable, instantiating it
instantiate1 :: Monad f => f a -> Scope () f a -> f a
instantiate1 e = instantiate (\ () -> e)
{-# INLINE instantiate1 #-}


-- | @fromScope@ quotients out the possible placements of F in Scope
-- distributing them all to the leaves. This yields a traditional deBruijn
-- indexing scheme for bound variables.
--
-- > fromScope . toScope = id
-- > fromScope . toScope . fromScope = fromScope
--
-- @(toScope . fromScope)@ is idempotent
fromScope :: Monad f => Scope b f a -> f (Var b a)
fromScope (Scope s) = s >>= \v -> case v of
  F e -> liftM F e
  B b -> return (B b)
{-# INLINE fromScope #-}

toScope :: Monad f => f (Var b a) -> Scope b f a
toScope e = Scope (liftM (fmap return) e)
{-# INLINE toScope #-}

-- | Perform substitution on both bound and free variables in a scope
splat :: Monad f => (a -> f c) -> (b -> f c) -> Scope b f a -> f c
splat f unbind s = unscope s >>= \v -> case v of
  B b -> unbind b
  F ea -> ea >>= f
{-# INLINE splat #-}
