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
-- This is the work-horse of the @bound@ library.
--
-- 'Scope' provides a single generalized de Bruijn level
-- and is often used inside of the definition of binders.
----------------------------------------------------------------------------
module Bound.Scope
  ( Scope(..)
  -- * Abstraction
  , abstract, abstract1
  -- * Instantiation
  , instantiate, instantiate1
  -- * Traditional de Bruijn
  , fromScope
  , toScope
  -- * Bound variable manipulation
  , splat
  , bindings
  , mapBound
  , mapScope
  , liftMBound
  , liftMScope
  , foldMapBound
  , foldMapScope
  , traverseBound_
  , traverseScope_
  , mapMBound_
  , mapMScope_
  , traverseBound
  , traverseScope
  , mapMBound
  , mapMScope
  ) where

import Bound.Class
import Bound.Var
import Control.Applicative
import Control.Monad hiding (mapM, mapM_)
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Prelude.Extras
import Prelude hiding (foldr, mapM, mapM_)

-------------------------------------------------------------------------------
-- Scopes
-------------------------------------------------------------------------------

-- | @'Scope' b f a@ is an @f@ expression with bound variables in @b@,
-- and free variables in @a@
--
-- We store bound variables as their generalized de Bruijn
-- representation in that we're allowed to 'lift' (using 'F') an entire
-- tree rather than only succ individual variables, but we're still
-- only allowed to do so once per 'Scope'. Weakening trees permits
-- /O(1)/ weakening and permits more sharing opportunities. Here the
-- deBruijn 0 is represented by the 'B' constructor of 'Var', while the
-- de Bruijn 'succ' (which may be applied to an entire tree!) is handled
-- by 'F'.
--
-- NB: equality and comparison quotient out the distinct 'F' placements
-- allowed by the generalized de Bruijn representation and return the
-- same result as a traditional de Bruijn representation would.
--
-- Logically you can think of this as if the shape were the traditional
-- @f (Var b a)@, but the extra @f a@ inside permits us a cheaper 'lift'.
--
newtype Scope b f a = Scope { unscope :: f (Var b (f a)) }

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor f => Functor (Scope b f) where
  fmap f (Scope a) = Scope (fmap (fmap (fmap f)) a)

-- | @'toList'@ is provides a list (with duplicates) of the free variables
instance Foldable f => Foldable (Scope b f) where
  foldMap f (Scope a) = foldMap (foldMap (foldMap f)) a

instance Traversable f => Traversable (Scope b f) where
  traverse f (Scope a) = Scope <$> traverse (traverse (traverse f)) a

-- | The monad permits substitution on free variables, while preserving
-- bound variables
instance Monad f => Monad (Scope b f) where
  return a = Scope (return (F (return a)))
  Scope e >>= f = Scope $ e >>= \v -> case v of
    B b -> return (B b)
    F ea -> ea >>= unscope . f

instance MonadTrans (Scope b) where
  lift m = Scope (return (F m))

instance (Monad f, Eq b, Eq1 f, Eq a) => Eq  (Scope b f a) where
  (==) = (==#)
instance (Monad f, Eq b, Eq1 f)       => Eq1 (Scope b f)   where
  a ==# b = liftM Lift2 (fromScope a) ==# liftM Lift2 (fromScope b)
  -- a ==# b = mangleScope a ==# mangleScope b

instance (Monad f, Ord b, Ord1 f, Ord a) => Ord  (Scope b f a) where
  compare = compare1
instance (Monad f, Ord b, Ord1 f)        => Ord1 (Scope b f) where
  compare1 a b = liftM Lift2 (fromScope a) `compare1` liftM Lift2 (fromScope b)
  -- compare1 a b = compare1 (mangleScope a) (mangleScope b)

instance (Functor f, Show b, Show1 f, Show a) => Show (Scope b f a) where
  showsPrec = showsPrec1
instance (Functor f, Show b, Show1 f) => Show1 (Scope b f) where
  showsPrec1 d a = showParen (d > 10) $
    showString "Scope " . showsPrec1 11 (fmap (Lift2 . fmap Lift1) (unscope a))

instance (Functor f, Read b, Read1 f, Read a) => Read  (Scope b f a) where
  readsPrec = readsPrec1
instance (Functor f, Read b, Read1 f)         => Read1 (Scope b f) where
  readsPrec1 d = readParen (d > 10) $ \r -> do
    ("Scope", r') <- lex r
    (s, r'') <- readsPrec1 11 r'
    return (Scope (fmap (fmap lower1 . lower2) s), r'')

instance Bound (Scope b) where
  Scope m >>>= f = Scope (liftM (fmap (>>= f)) m)

-------------------------------------------------------------------------------
-- Abstraction
-------------------------------------------------------------------------------

-- | Capture some free variables in an expression to yield
-- a 'Scope' with bound variables in @b@
--
-- >>> :m + Data.List
-- >>> abstract (`elemIndex` "bar") "barry"
-- Scope [B 0,B 1,B 2,B 2,F "y"]
abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract f e = Scope (liftM k e) where
  k y = case f y of
    Just z  -> B z
    Nothing -> F (return y)
{-# INLINE abstract #-}

-- | Abstract over a single variable
--
-- >>> abstract1 'x' "xyz"
-- Scope [B (),F "y",F "z"]
abstract1 :: (Monad f, Eq a) => a -> f a -> Scope () f a
abstract1 a = abstract (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1 #-}

-------------------------------------------------------------------------------
-- Instantiation
-------------------------------------------------------------------------------

-- | Enter a scope, instantiating all bound variables
--
-- >>> :m + Data.List
-- >>> instantiate (\x -> [toEnum (97 + x)]) $ abstract (`elemIndex` "bar") "barry"
-- "abccy"
instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
instantiate k e = unscope e >>= \v -> case v of
  B b -> k b
  F a -> a
{-# INLINE instantiate #-}

-- | Enter a 'Scope' that binds one variable, instantiating it
--
-- >>> instantiate1 "x" $ Scope [B (),F "y",F "z"]
-- "xyz"
instantiate1 :: Monad f => f a -> Scope () f a -> f a
instantiate1 e = instantiate (const e)
{-# INLINE instantiate1 #-}

-------------------------------------------------------------------------------
-- Traditional de Bruijn
-------------------------------------------------------------------------------

-- | @'fromScope'@ quotients out the possible placements of 'F' in 'Scope'
-- by distributing them all to the leaves. This yields a more traditional
-- de Bruijn indexing scheme for bound variables.
--
-- Since,
--
-- @'fromScope' '.' 'toScope' ≡ 'id'@
--
-- we know that
--
-- @'fromScope' '.' 'toScope' '.' 'fromScope' ≡ 'fromScope'@
--
-- and therefore @('toScope' . 'fromScope')@ is idempotent.
fromScope :: Monad f => Scope b f a -> f (Var b a)
fromScope (Scope s) = s >>= \v -> case v of
  F e -> liftM F e
  B b -> return (B b)
{-# INLINE fromScope #-}

-- | Convert from traditional de Bruijn to generalized de Bruijn indices.
--
-- This requires a full tree traversal
toScope :: Monad f => f (Var b a) -> Scope b f a
toScope e = Scope (liftM (fmap return) e)
{-# INLINE toScope #-}

-------------------------------------------------------------------------------
-- Exotic Traversals of Bound Variables (not exported by default)
-------------------------------------------------------------------------------

-- | Perform substitution on both bound and free variables in a 'Scope'.
splat :: Monad f => (a -> f c) -> (b -> f c) -> Scope b f a -> f c
splat f unbind s = unscope s >>= \v -> case v of
  B b -> unbind b
  F ea -> ea >>= f
{-# INLINE splat #-}

-- | Return a list of occurences of the variables bound by this 'Scope'.
bindings :: Foldable f => Scope b f a -> [b]
bindings (Scope s) = foldr f [] s where
  f (B v) vs = v : vs
  f _ vs     = vs
{-# INLINE bindings #-}

-- | Perform a change of variables on bound variables.
mapBound :: Functor f => (b -> b') -> Scope b f a -> Scope b' f a
mapBound f (Scope s) = Scope (fmap f' s) where
  f' (B b) = B (f b)
  f' (F a) = F a
{-# INLINE mapBound #-}

-- | Perform a change of variables, reassigning both bound and free variables.
mapScope :: Functor f => (b -> d) -> (a -> c) -> Scope b f a -> Scope d f c
mapScope f g (Scope s) = Scope $ fmap (bimap f (fmap g)) s
{-# INLINE mapScope #-}

-- | Perform a change of variables on bound variables given only a 'Monad'
-- instance
liftMBound :: Monad m => (b -> b') -> Scope b m a -> Scope b' m a
liftMBound f (Scope s) = Scope (liftM f' s) where
  f' (B b) = B (f b)
  f' (F a) = F a
{-# INLINE liftMBound #-}

-- | A version of 'mapScope' that can be used when you only have the 'Monad'
-- instance
liftMScope :: Monad m => (b -> d) -> (a -> c) -> Scope b m a -> Scope d m c
liftMScope f g (Scope s) = Scope $ liftM (bimap f (liftM g)) s
{-# INLINE liftMScope #-}

-- | Obtain a result by collecting information from both bound and free
-- variables
foldMapBound :: (Foldable f, Monoid r) => (b -> r) -> Scope b f a -> r
foldMapBound f (Scope s) = foldMap f' s where
  f' (B a) = f a
  f' _     = mempty
{-# INLINE foldMapBound #-}

-- | Obtain a result by collecting information from both bound and free
-- variables
foldMapScope :: (Foldable f, Monoid r) =>
                (b -> r) -> (a -> r) -> Scope b f a -> r
foldMapScope f g (Scope s) = foldMap (bifoldMap f (foldMap g)) s
{-# INLINE foldMapScope #-}

-- | 'traverse_' the bound variables in a 'Scope'.
traverseBound_ :: (Applicative g, Foldable f) =>
                  (b -> g d) -> Scope b f a -> g ()
traverseBound_ f (Scope s) = traverse_ f' s
  where f' (B a) = () <$ f a
        f' _     = pure ()
{-# INLINE traverseBound_ #-}

-- | 'traverse' both the variables bound by this scope and any free variables.
traverseScope_ :: (Applicative g, Foldable f) =>
                  (b -> g d) -> (a -> g c) -> Scope b f a -> g ()
traverseScope_ f g (Scope s) = traverse_ (bitraverse_ f (traverse_ g)) s
{-# INLINE traverseScope_ #-}

-- | mapM_ over the variables bound by this scope
mapMBound_ :: (Monad g, Foldable f) => (b -> g d) -> Scope b f a -> g ()
mapMBound_ f (Scope s) = mapM_ f' s where
  f' (B a) = do _ <- f a; return ()
  f' _     = return ()
{-# INLINE mapMBound_ #-}

-- | A 'traverseScope_' that can be used when you only have a 'Monad'
-- instance
mapMScope_ :: (Monad m, Foldable f) =>
              (b -> m d) -> (a -> m c) -> Scope b f a -> m ()
mapMScope_ f g (Scope s) = mapM_ (bimapM_ f (mapM_ g)) s
{-# INLINE mapMScope_ #-}

-- | Traverse both bound and free variables
traverseBound :: (Applicative g, Traversable f) =>
                 (b -> g c) -> Scope b f a -> g (Scope c f a)
traverseBound f (Scope s) = Scope <$> traverse f' s where
  f' (B b) = B <$> f b
  f' (F a) = pure (F a)
{-# INLINE traverseBound #-}

-- | Traverse both bound and free variables
traverseScope :: (Applicative g, Traversable f) =>
                 (b -> g d) -> (a -> g c) -> Scope b f a -> g (Scope d f c)
traverseScope f g (Scope s) = Scope <$> traverse (bitraverse f (traverse g)) s
{-# INLINE traverseScope #-}

-- | mapM over both bound and free variables
mapMBound :: (Monad m, Traversable f) =>
             (b -> m c) -> Scope b f a -> m (Scope c f a)
mapMBound f (Scope s) = liftM Scope (mapM f' s) where
  f' (B b) = liftM B (f b)
  f' (F a) = return (F a)
{-# INLINE mapMBound #-}

-- | A 'traverseScope' that can be used when you only have a 'Monad'
-- instance
mapMScope :: (Monad m, Traversable f) =>
             (b -> m d) -> (a -> m c) -> Scope b f a -> m (Scope d f c)
mapMScope f g (Scope s) = liftM Scope (mapM (bimapM f (mapM g)) s)
{-# INLINE mapMScope #-}

