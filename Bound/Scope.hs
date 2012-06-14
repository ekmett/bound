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
  , substitute
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
import Text.Read hiding (lift)

-- | @Scope b f a@ is a an f expression with bound variables b, and free variables a
--
-- This stores bound variables as their generalized deBruijn representation,
-- in that the succ's for variable ids are allowed to occur anywhere within the tree
-- permitting O(1) weakening and allowing more sharing opportunities.

newtype Scope b f a = Scope { unscope :: f (Var b (f a)) }

instance Functor f => Functor (Scope b f) where
  fmap f (Scope a) = Scope (fmap (fmap (fmap f)) a)

instance Foldable f => Foldable (Scope b f) where
  foldMap f (Scope a) = foldMap (foldMap (foldMap f)) a

instance Traversable f => Traversable (Scope b f) where
  traverse f (Scope a) = Scope <$> traverse (traverse (traverse f)) a

-- | The monad permits substitution on free variables, while preserving bound variables
instance Monad f => Monad (Scope b f) where
  return a = Scope (return (Free (return a)))
  Scope e >>= f = Scope $ e >>= \v -> case v of
    Bound b -> return (Bound b)
    Free ea -> ea >>= unscope . f

instance MonadTrans (Scope b) where
  lift m = Scope (return (Free m))

mangleScope :: Functor f => Scope b f a -> Lift1 f (Lift2 Var b (Lift1 f a))
mangleScope (Scope a) = Lift1 (fmap (Lift2 . fmap Lift1) a)
{-# INLINE mangleScope #-}

unmangleScope :: Functor f => Lift1 f (Lift2 Var b (Lift1 f a)) -> Scope b f a
unmangleScope (Lift1 a) = Scope (fmap (fmap lower1 . lower2) a)
{-# INLINE unmangleScope #-}

instance (Functor f, Eq b, Eq1 f, Eq a)       => Eq   (Scope b f a) where
  a == b = mangleScope a == mangleScope b

instance (Functor f, Ord b, Ord1 f, Ord a)    => Ord  (Scope b f a) where
  compare a b = compare (mangleScope a) (mangleScope b)

instance (Functor f, Show b, Show1 f, Show a) => Show (Scope b f a) where
  showsPrec d a = showsPrec d (mangleScope a)

instance (Functor f, Read b, Read1 f, Read a) => Read (Scope b f a) where
  readPrec = liftM unmangleScope readPrec

instance Bound (Scope b) where
  m >>>= f = m >>= lift . f

-- | capture some free variables in an expression to yield a Scope with bound variables
abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract f e = Scope (liftM k e) where
  k y = case f y of
    Just z  -> Bound z
    Nothing -> Free (return y)
{-# INLINE abstract #-}

-- | abstract over a single variable
abstract1 :: (Monad f, Eq a) => a -> f a -> Scope () f a
abstract1 a = abstract (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1 #-}

-- | enter a scope, instantiating all bound variables
instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
instantiate k e = unscope e >>= \v -> case v of
  Bound b -> k b
  Free a -> a
{-# INLINE instantiate #-}

-- | enter a scope with one bound variable, instantiating it
instantiate1 :: Monad f => f a -> Scope () f a -> f a
instantiate1 e = instantiate (\ () -> e)
{-# INLINE instantiate1 #-}

-- | @substitute p a w@ replaces the free variable @a@ with @p@ in @w@
substitute :: (Monad f, Eq a) => f a -> a -> f a -> f a
substitute p a w = w >>= \b -> if a == b then p else return b
{-# INLINE substitute #-}

-- | @fromScope@ quotients out the various placements of Free in Scope
-- distributing them all to the leaves. This yields a traditional deBruijn
-- indexing scheme for bound variables.
--
-- > fromScope . toScope = id
-- > fromScope . toScope . fromScope = fromScope
--
-- @(toScope . fromScope)@ is idempotent
fromScope :: Monad f => Scope b f a -> f (Var b a)
fromScope (Scope s) = s >>= \v -> case v of
  Free e -> liftM Free e
  Bound b -> return (Bound b)
{-# INLINE fromScope #-}

toScope :: Monad f => f (Var b a) -> Scope b f a
toScope e = Scope (liftM (fmap return) e)
{-# INLINE toScope #-}

splat :: Monad f => (a -> f c) -> (b -> f c) -> Scope b f a -> f c
splat f unbind s = unscope s >>= \v -> case v of
  Bound b -> unbind b
  Free ea -> ea >>= f
