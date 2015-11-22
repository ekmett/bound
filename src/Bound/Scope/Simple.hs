{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- 'Scope' provides a single traditional de Bruijn level
-- and is often used inside of the definition of binders.
--
----------------------------------------------------------------------------
module Bound.Scope.Simple
  (Scope(..)
  -- * Abstraction
  , abstract, abstract1
  -- * Instantiation
  , instantiate, instantiate1
  -- * Alternative names for 'unscope'/'Scope'
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
  , serializeScope
  , deserializeScope
  , hoistScope
  , bitraverseScope
  , bitransverseScope
  , transverseScope
  , instantiateVars
  ) where

import Bound.Class
import Bound.Var
import Control.Applicative
import Control.Monad hiding (mapM, mapM_)
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Bifoldable
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bitraversable
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Data
import Data.Foldable
import Data.Hashable
import Data.Hashable.Extras
import Data.Monoid
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Traversable
import Prelude.Extras
import Prelude hiding (foldr, mapM, mapM_)

-------------------------------------------------------------------------------
-- Scopes
-------------------------------------------------------------------------------

-- | @'Scope' b f a@ is an @f@ expression with bound variables in @b@,
-- and free variables in @a@
--
-- This implements traditional de Bruijn indices, while 'Bound.Scope'
-- implements generalized de Bruijn indices.
--
-- These traditional indices can be used to test the performance gain
-- of generalized indices.
--
-- While this type 'Scope' is identical to 'Control.Monad.Trans.EitherT'
-- this module focuses on a drop-in replacement for 'Bound.Scope'.
--
-- Another use case is for syntaxes not stable under substitution,
-- therefore with only a 'Functor' instance and no 'Monad' instance.
newtype Scope b f a = Scope { unscope :: f (Var b a) }
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 707
  deriving Typeable
#endif

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor f => Functor (Scope b f) where
  fmap f (Scope a) = Scope (fmap (fmap f) a)
  {-# INLINE fmap #-}

-- | @'toList'@ is provides a list (with duplicates) of the free variables
instance Foldable f => Foldable (Scope b f) where
  foldMap f (Scope a) = foldMap (foldMap f) a
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Scope b f) where
  traverse f (Scope a) = Scope <$> traverse (traverse f) a
  {-# INLINE traverse #-}

instance (Functor f, Monad f) => Applicative (Scope b f) where
  pure a = Scope (return (F a))
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | The monad permits substitution on free variables, while preserving
-- bound variables
instance Monad f => Monad (Scope b f) where
#if __GLASGOW_HASKELL__ < 710
  return a = Scope (return (F a))
  {-# INLINE return #-}
#endif
  Scope e >>= f = Scope $ e >>= \v -> case v of
    B b -> return (B b)
    F a -> unscope (f a)
  {-# INLINE (>>=) #-}

instance MonadTrans (Scope b) where
  lift ma = Scope (liftM F ma)
  {-# INLINE lift #-}

instance (Functor f, Eq b, Eq1 f, Eq a) => Eq  (Scope b f a) where
  (==) = (==#)
  {-# INLINE (==) #-}
instance (Functor f, Eq b, Eq1 f)       => Eq1 (Scope b f)   where
  a ==# b = unscope a ==# unscope b
  {-# INLINE (==#) #-}

instance (Functor f, Ord b, Ord1 f, Ord a) => Ord  (Scope b f a) where
  compare = compare1
  {-# INLINE compare #-}
instance (Functor f, Ord b, Ord1 f)        => Ord1 (Scope b f) where
  compare1 a b = unscope a `compare1` unscope b
  {-# INLINE compare1 #-}

instance (Functor f, Show b, Show1 f, Show a) => Show (Scope b f a) where
  showsPrec = showsPrec1
instance (Functor f, Show b, Show1 f) => Show1 (Scope b f) where
  showsPrec1 d a = showParen (d > 10) $
    showString "Scope " . showsPrec1 11 (unscope a)

instance (Functor f, Read b, Read1 f, Read a) => Read  (Scope b f a) where
  readsPrec = readsPrec1
instance (Functor f, Read b, Read1 f)         => Read1 (Scope b f) where
  readsPrec1 d = readParen (d > 10) $ \r -> do
    ("Scope", r') <- lex r
    (s, r'') <- readsPrec1 11 r'
    return (Scope (fmap lower1 s), r'')

instance Bound (Scope b) where
  Scope m >>>= f = Scope $ m >>= \v -> case v of
    B b -> return (B b)
    F a -> liftM F (f a)
  {-# INLINE (>>>=) #-}

instance (Hashable b, Monad f, Hashable1 f) => Hashable1 (Scope b f) where
  hashWithSalt1 n m = hashWithSalt1 n (unscope m)
  {-# INLINE hashWithSalt1 #-}

instance (Hashable b, Monad f, Hashable1 f, Hashable a) => Hashable (Scope b f a) where
  hashWithSalt n m = hashWithSalt1 n (unscope m)
  {-# INLINE hashWithSalt #-}

-------------------------------------------------------------------------------
-- Abstraction
-------------------------------------------------------------------------------

-- | Capture some free variables in an expression to yield
-- a 'Scope' with bound variables in @b@
--
-- >>> :m + Data.List
-- >>> abstract (`elemIndex` "bar") "barry"
-- Scope [B 0,B 1,B 2,B 2,F 'y']
abstract :: Functor f => (a -> Maybe b) -> f a -> Scope b f a
abstract f e = Scope (fmap k e) where
  k y = case f y of
    Just z  -> B z
    Nothing -> F y
{-# INLINE abstract #-}

-- | Abstract over a single variable
--
-- >>> abstract1 'x' "xyz"
-- Scope [B (),F 'y',F 'z']
abstract1 :: (Functor f, Eq a) => a -> f a -> Scope () f a
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
  F a -> return a
{-# INLINE instantiate #-}

-- | Enter a 'Scope' that binds one variable, instantiating it
--
-- >>> instantiate1 "x" $ Scope [B (),F 'y',F 'z']
-- "xyz"
instantiate1 :: Monad f => f a -> Scope n f a -> f a
instantiate1 e = instantiate (const e)
{-# INLINE instantiate1 #-}

hoistScope :: (f (Var b a) -> g (Var b a)) -> Scope b f a -> Scope b g a
hoistScope f = Scope . f . unscope
{-# INLINE hoistScope #-}

-------------------------------------------------------------------------------
-- Compatibility with Bound.Scope
-------------------------------------------------------------------------------

-- | @'fromScope'@ is just another name for 'unscope' and is exported
-- to mimick 'Bound.Scope.fromScope'.
-- In particular no 'Monad' constraint is required.
fromScope :: Scope b f a -> f (Var b a)
fromScope = unscope
{-# INLINE fromScope #-}

-- | @'toScope'@ is just another name for 'Scope' and is exported
-- to mimick 'Bound.Scope.toScope'.
-- In particular no 'Monad' constraint is required.
toScope :: f (Var b a) -> Scope b f a
toScope = Scope
{-# INLINE toScope #-}

-------------------------------------------------------------------------------
-- Exotic Traversals of Bound Variables (not exported by default)
-------------------------------------------------------------------------------

-- | Perform substitution on both bound and free variables in a 'Scope'.
splat :: Monad f => (a -> f c) -> (b -> f c) -> Scope b f a -> f c
splat f unbind s = unscope s >>= \v -> case v of
  B b -> unbind b
  F a -> f a
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
mapScope f g (Scope s) = Scope $ fmap (bimap f g) s
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
liftMScope f g (Scope s) = Scope $ liftM (bimap f g) s
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
foldMapScope f g (Scope s) = foldMap (bifoldMap f g) s
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
traverseScope_ f g (Scope s) = traverse_ (bitraverse_ f g) s
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
mapMScope_ f g (Scope s) = mapM_ (bimapM_ f g) s
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
traverseScope f g (Scope s) = Scope <$> traverse (bitraverse f g) s
{-# INLINE traverseScope #-}

-- | This allows you to 'bitraverse' a 'Scope'.
bitraverseScope :: (Bitraversable t, Applicative f) => (k -> f k') -> (a -> f a') -> Scope b (t k) a -> f (Scope b (t k') a')
bitraverseScope f = bitransverseScope (bitraverse f)
{-# INLINE bitraverseScope #-}

-- | This is a higher-order analogue of 'traverse'.
transverseScope :: (Functor f)
                => (forall r. g r -> f (h r))
                -> Scope b g a -> f (Scope b h a)
transverseScope tau (Scope s) = Scope <$> tau s

-- | instantiate bound variables using a list of new variables
instantiateVars :: Monad t => [a] -> Scope Int t a -> t a
instantiateVars as = instantiate (vs !!) where
  vs = map return as
{-# INLINE instantiateVars #-}

bitransverseScope :: Applicative f => (forall a a'. (a -> f a') ->         t a -> f         (u a'))
                                   ->  forall a a'. (a -> f a') -> Scope b t a -> f (Scope b u a')
bitransverseScope tau f (Scope s) = Scope <$> tau (traverse f) s
{-# INLINE bitransverseScope #-}

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
mapMScope f g (Scope s) = liftM Scope (mapM (bimapM f g) s)
{-# INLINE mapMScope #-}

serializeScope :: (Serial1 f, MonadPut m) => (b -> m ()) -> (v -> m ()) -> Scope b f v -> m ()
serializeScope pb pv (Scope body) = serializeWith (serializeWith2 pb pv) body
{-# INLINE serializeScope #-}

deserializeScope :: (Serial1 f, MonadGet m) => m b -> m v -> m (Scope b f v)
deserializeScope gb gv = liftM Scope $ deserializeWith (deserializeWith2 gb gv)
{-# INLINE deserializeScope #-}

instance (Serial b, Serial1 f) => Serial1 (Scope b f) where
  serializeWith = serializeScope serialize
  deserializeWith = deserializeScope deserialize

instance (Serial b, Serial1 f, Serial a) => Serial (Scope b f a) where
  serialize = serializeScope serialize serialize
  deserialize = deserializeScope deserialize deserialize

instance (Binary b, Serial1 f, Binary a) => Binary (Scope b f a) where
  put = serializeScope Binary.put Binary.put
  get = deserializeScope Binary.get Binary.get

instance (Serialize b, Serial1 f, Serialize a) => Serialize (Scope b f a) where
  put = serializeScope Serialize.put Serialize.put
  get = deserializeScope Serialize.get Serialize.get

#ifdef __GLASGOW_HASKELL__

#if __GLASGOW_HASKELL__ < 707
instance (Typeable b, Typeable1 f) => Typeable1 (Scope b f) where
  typeOf1 _ = mkTyConApp scopeTyCon [typeOf (undefined :: b), typeOf1 (undefined :: f ())]

scopeTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
scopeTyCon = mkTyCon3 "bound" "Bound.Scope" "Scope"
#else
scopeTyCon = mkTyCon "Bound.Scope.Scope"
#endif

#else

-- only needed for ghc7.8.1rc1 compatibility
#define Typeable1 Typeable

#endif

deriving instance (Typeable b, Typeable1 f, Data a, Data (f (Var b a))) => Data (Scope b f a)

#endif
