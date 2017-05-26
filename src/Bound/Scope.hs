{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
#ifdef __GLASGOW_HASKELL__
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
-- Copyright   :  (C) 2012-2013 Edward Kmett
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
  , abstract, abstractE, abstract1, abstractAll
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
import Control.DeepSeq
import Control.Monad hiding (mapM, mapM_)
import Control.Monad.Morph
import Data.Bifunctor
import Data.Bifoldable
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bitraversable
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Foldable
import Data.Functor.Classes
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1(..), hashWithSalt1)
import Data.Monoid
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Traversable
import Prelude hiding (foldr, mapM, mapM_)
import Data.Data

-- $setup
-- >>> import Bound.Var

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
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  deriving Typeable
#endif

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor f => Functor (Scope b f) where
  fmap f (Scope a) = Scope (fmap (fmap (fmap f)) a)
  {-# INLINE fmap #-}

-- | @'toList'@ is provides a list (with duplicates) of the free variables
instance Foldable f => Foldable (Scope b f) where
  foldMap f (Scope a) = foldMap (foldMap (foldMap f)) a
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Scope b f) where
  traverse f (Scope a) = Scope <$> traverse (traverse (traverse f)) a
  {-# INLINE traverse #-}

instance (Functor f, Monad f) => Applicative (Scope b f) where
  pure a = Scope (return (F (return a)))
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | The monad permits substitution on free variables, while preserving
-- bound variables
instance Monad f => Monad (Scope b f) where
#if __GLASGOW_HASKELL__ < 710
  return a = Scope (return (F (return a)))
  {-# INLINE return #-}
#endif
  Scope e >>= f = Scope $ e >>= \v -> case v of
    B b -> return (B b)
    F ea -> ea >>= unscope . f
  {-# INLINE (>>=) #-}

instance MonadTrans (Scope b) where
  lift m = Scope (return (F m))
  {-# INLINE lift #-}

instance MFunctor (Scope b) where
#if __GLASGOW_HASKELL__ < 710
  hoist t (Scope b) = Scope $ t (liftM (liftM t) b)
#else
  hoist = hoistScope
#endif
  {-# INLINE hoist #-}

instance (Monad f, Eq b, Eq1 f, Eq a) => Eq  (Scope b f a) where (==) = eq1
instance (Monad f, Ord b, Ord1 f, Ord a) => Ord  (Scope b f a) where compare = compare1

#if MIN_VERSION_transformers(0,5,0) || !(MIN_VERSION_transformers(0,4,0))

--------------------------------------------------------------------------------
-- * transformers 0.5 Data.Functor.Classes
--------------------------------------------------------------------------------

instance (Read b, Read1 f, Read a) => Read  (Scope b f a) where readsPrec = readsPrec1
instance (Show b, Show1 f, Show a) => Show (Scope b f a) where showsPrec = showsPrec1

instance (Monad f, Eq b, Eq1 f) => Eq1 (Scope b f) where
  liftEq f m n = liftEq (liftEq f) (fromScope m) (fromScope n)

instance (Monad f, Ord b, Ord1 f) => Ord1 (Scope b f) where
  liftCompare f m n = liftCompare (liftCompare f) (fromScope m) (fromScope n)

instance (Show b, Show1 f) => Show1 (Scope b f) where
  liftShowsPrec f g d m = showsUnaryWith (liftShowsPrec (liftShowsPrec f' g') (liftShowList f' g')) "Scope" d (unscope m) where
    f' = liftShowsPrec f g
    g' = liftShowList f g

instance (Read b, Read1 f) => Read1 (Scope b f) where
  liftReadsPrec f g = readsData $ readsUnaryWith (liftReadsPrec (liftReadsPrec f' g') (liftReadList f' g')) "Scope" Scope where
    f' = liftReadsPrec f g
    g' = liftReadList f g

#else

--------------------------------------------------------------------------------
-- * transformers 0.4 Data.Functor.Classes
--------------------------------------------------------------------------------

instance (Functor f, Read b, Read1 f, Read a) => Read  (Scope b f a) where readsPrec = readsPrec1
instance (Functor f, Show b, Show1 f, Show a) => Show (Scope b f a) where showsPrec = showsPrec1

instance (Monad f, Eq b, Eq1 f) => Eq1 (Scope b f) where
  eq1 a b = eq1 (fromScope a) (fromScope b)

instance (Monad f, Ord b, Ord1 f) => Ord1 (Scope b f) where
  compare1 a b = fromScope a `compare1` fromScope b

newtype Lift1 f a = Lift1 { lower1 :: f a }
instance (Show1 f, Show a) => Show (Lift1 f a) where showsPrec d (Lift1 m) = showsPrec1 d m
instance (Read1 f, Read a) => Read (Lift1 f a) where
    readsPrec d m = fmap (first Lift1) $ readsPrec1 d m

instance (Functor f, Show b, Show1 f) => Show1 (Scope b f) where
  showsPrec1 d a = showParen (d > 10) $
    showString "Scope " . showsPrec1 11 (fmap (fmap Lift1) (unscope a))

instance (Functor f, Read b, Read1 f) => Read1 (Scope b f) where
  readsPrec1 d = readParen (d > 10) $ \r -> do
    ("Scope", r') <- lex r
    (s, r'') <- readsPrec1 11 r'
    return (Scope (fmap (fmap lower1) s), r'')
#endif

instance Bound (Scope b) where
  Scope m >>>= f = Scope (liftM (fmap (>>= f)) m)
  {-# INLINE (>>>=) #-}

--  {-# INLINE hashWithSalt1 #-}

instance (Hashable b, Monad f, Hashable1 f) => Hashable1 (Scope b f) where
  liftHashWithSalt h s m = liftHashWithSalt (liftHashWithSalt h) s (fromScope m)
  {-# INLINE liftHashWithSalt #-}

instance (Hashable b, Monad f, Hashable1 f, Hashable a) => Hashable (Scope b f a) where
  hashWithSalt n m = hashWithSalt1 n (fromScope m)
  {-# INLINE hashWithSalt #-}

instance NFData (f (Var b (f a))) => NFData (Scope b f a) where
  rnf scope = rnf (unscope scope)

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

-- | Capture some free variables in an expression to yield
-- a 'Scope' with bound variables in @b@. Optionally change the
-- types of the remaining free variables.
abstractE :: Monad f => (a -> Either a' b) -> f a -> Scope b f a'
abstractE f e = Scope (liftM k e) where
  k y = case f y of
    Right z -> B z
    Left y' -> F (return y')

-- | Capture all the free variables in an expression to yield
-- a 'Scope' with bound variables in @b@.
abstractAll :: Monad f => (a -> b) -> f a -> Scope b f c
abstractAll f = abstractE (Right . f)

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
instantiate1 :: Monad f => f a -> Scope n f a -> f a
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

-- | Obtain a result by collecting information from bound variables
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

-- | 'traverse' the bound variables in a 'Scope'.
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

serializeScope :: (Serial1 f, MonadPut m) => (b -> m ()) -> (v -> m ()) -> Scope b f v -> m ()
serializeScope pb pv (Scope body) = serializeWith (serializeWith2 pb $ serializeWith pv) body
{-# INLINE serializeScope #-}

deserializeScope :: (Serial1 f, MonadGet m) => m b -> m v -> m (Scope b f v)
deserializeScope gb gv = liftM Scope $ deserializeWith (deserializeWith2 gb $ deserializeWith gv)
{-# INLINE deserializeScope #-}

-- | This allows you to 'bitraverse' a 'Scope'.
bitraverseScope :: (Bitraversable t, Applicative f) => (k -> f k') -> (a -> f a') -> Scope b (t k) a -> f (Scope b (t k') a')
bitraverseScope f = bitransverseScope (bitraverse f)
{-# INLINE bitraverseScope #-}

-- | This is a higher-order analogue of 'traverse'.
transverseScope :: (Applicative f, Monad f, Traversable g)
                => (forall r. g r -> f (h r))
                -> Scope b g a -> f (Scope b h a)
transverseScope tau (Scope e) = Scope <$> (tau =<< traverse (traverse tau) e)

bitransverseScope :: Applicative f => (forall a a'. (a -> f a') -> t a -> f (u a')) -> (c -> f c') -> Scope b t c -> f (Scope b u c')
bitransverseScope tau f = fmap Scope . tau (_F (tau f)) . unscope
{-# INLINE bitransverseScope #-}

-- | instantiate bound variables using a list of new variables
instantiateVars :: Monad t => [a] -> Scope Int t a -> t a
instantiateVars as = instantiate (vs !!) where
  vs = map return as
{-# INLINE instantiateVars #-}

-- | Lift a natural transformation from @f@ to @g@ into one between scopes.
hoistScope :: Functor f => (forall x. f x -> g x) -> Scope b f a -> Scope b g a
hoistScope t (Scope b) = Scope $ t (fmap t <$> b)
{-# INLINE hoistScope #-}

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
#define Typeable1 Typeable
#endif

deriving instance (Typeable b, Typeable1 f, Data a, Data (f (Var b (f a)))) => Data (Scope b f a)

#endif
