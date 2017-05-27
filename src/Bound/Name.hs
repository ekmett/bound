{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}

# if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DeriveGeneric #-}
# endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#endif
-----------------------------------------------------------------------------
-- |
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
  , _Name
  , name
  , abstractName
  , abstract1Name
  , abstractEitherName
  , instantiateName
  , instantiate1Name
  , instantiateEitherName
  ) where

import Bound.Scope
import Bound.Var
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.DeepSeq
import Control.Monad (liftM, liftM2)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif
import Data.Bifunctor
import Data.Bifoldable
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bitraversable
import Data.Bytes.Serial
import Data.Functor.Classes
#ifdef __GLASGOW_HASKELL__
import Data.Data
# if __GLASGOW_HASKELL__ >= 704
import GHC.Generics
# endif
#endif
import Data.Hashable (Hashable(..))
import Data.Hashable.Lifted (Hashable1(..), Hashable2(..))
import Data.Profunctor
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)

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
{-# INLINE name #-}

-- |
--
-- This provides an 'Iso' that can be used to access the parts of a 'Name'.
--
-- @
-- '_Name' :: Iso ('Name' n a) ('Name' m b) (n, a) (m, b)
-- @
_Name :: (Profunctor p, Functor f) => p (n, a) (f (m,b)) -> p (Name n a) (f (Name m b))
_Name = dimap (\(Name n a) -> (n, a)) (fmap (uncurry Name))
{-# INLINE _Name #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Eq b => Eq (Name n b) where
  Name _ a == Name _ b = a == b
  {-# INLINE (==) #-}

instance Hashable2 Name where
  liftHashWithSalt2 _ h s (Name _ a) = h s a
  {-# INLINE liftHashWithSalt2 #-}

instance Hashable1 (Name n) where
  liftHashWithSalt h s (Name _ a) = h s a
  {-# INLINE liftHashWithSalt #-}

instance Hashable a => Hashable (Name n a) where
  hashWithSalt m (Name _ a) = hashWithSalt m a
  {-# INLINE hashWithSalt #-}

instance Ord b => Ord (Name n b) where
  Name _ a `compare` Name _ b = compare a b
  {-# INLINE compare #-}

instance Functor (Name n) where
  fmap f (Name n a) = Name n (f a)
  {-# INLINE fmap #-}

instance Foldable (Name n) where
  foldMap f (Name _ a) = f a
  {-# INLINE foldMap #-}

instance Traversable (Name n) where
  traverse f (Name n a) = Name n <$> f a
  {-# INLINE traverse #-}

instance Bifunctor Name where
  bimap f g (Name n a) = Name (f n) (g a)
  {-# INLINE bimap #-}

instance Bifoldable Name where
  bifoldMap f g (Name n a) = f n `mappend` g a
  {-# INLINE bifoldMap #-}

instance Bitraversable Name where
  bitraverse f g (Name n a) = Name <$> f n <*> g a
  {-# INLINE bitraverse #-}

instance Comonad (Name n) where
  extract (Name _ b) = b
  {-# INLINE extract #-}
  extend f w@(Name n _) = Name n (f w)
  {-# INLINE extend #-}

#if MIN_VERSION_transformers(0,5,0) || !MIN_VERSION_transformers(0,4,0)

instance Eq2 Name where
  liftEq2 _ g (Name _ b) (Name _ d) = g b d

instance Ord2 Name where
  liftCompare2 _ g (Name _ b) (Name _ d) = g b d

instance Show2 Name where
  liftShowsPrec2 f _ h _ d (Name a b) = showsBinaryWith f h "Name" d a b

instance Read2 Name where
  liftReadsPrec2 f _ h _ = readsData $ readsBinaryWith f h "Name" Name

instance Eq1 (Name b) where
  liftEq f (Name _ b) (Name _ d) = f b d

instance Ord1 (Name b) where
  liftCompare f (Name _ b) (Name _ d) = f b d

instance Show b => Show1 (Name b) where
  liftShowsPrec f _ d (Name a b) = showsBinaryWith showsPrec f "Name" d a b

instance Read b => Read1 (Name b) where
  liftReadsPrec f _ = readsData $ readsBinaryWith readsPrec f "Name" Name

#else

instance Eq1   (Name b) where eq1 = (==)
instance Ord1  (Name b) where compare1 = compare
instance Show b => Show1 (Name b) where showsPrec1 = showsPrec
instance Read b => Read1 (Name b) where readsPrec1 = readsPrec

--instance Eq2 Name   where eq2 = (==)
--instance Ord2 Name  where compare2   = compare
--instance Show2 Name where showsPrec2 = showsPrec
--instance Read2 Name where readsPrec2  = readsPrec

#endif

instance Serial2 Name where
  serializeWith2 pb pf (Name b a) = pb b >> pf a
  {-# INLINE serializeWith2 #-}

  deserializeWith2 = liftM2 Name
  {-# INLINE deserializeWith2 #-}

instance Serial b => Serial1 (Name b) where
  serializeWith = serializeWith2 serialize
  {-# INLINE serializeWith #-}
  deserializeWith = deserializeWith2 deserialize
  {-# INLINE deserializeWith #-}

instance (Serial b, Serial a) => Serial (Name b a) where
  serialize = serializeWith2 serialize serialize
  {-# INLINE serialize #-}
  deserialize = deserializeWith2 deserialize deserialize
  {-# INLINE deserialize #-}

instance (Binary b, Binary a) => Binary (Name b a) where
  put = serializeWith2 Binary.put Binary.put
  get = deserializeWith2 Binary.get Binary.get

instance (Serialize b, Serialize a) => Serialize (Name b a) where
  put = serializeWith2 Serialize.put Serialize.put
  get = deserializeWith2 Serialize.get Serialize.get

# if __GLASGOW_HASKELL__ >= 704
instance (NFData b, NFData a) => NFData (Name b a)
# endif

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

-- | Capture some free variables in an expression to yield
-- a 'Scope' with named bound variables. Optionally change the
-- types of the remaining free variables.
abstractEitherName :: Monad f => (a -> Either b c) -> f a -> Scope (Name a b) f c
abstractEitherName f e = Scope (liftM k e) where
  k y = case f y of
    Left z -> B (Name y z)
    Right y' -> F (return y')

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

instantiateEitherName :: (Monad f, Comonad n) => (Either b a -> f c) -> Scope (n b) f a -> f c
instantiateEitherName k e = unscope e >>= \v -> case v of
  B b -> k (Left (extract b))
  F a -> a >>= k . Right
{-# INLINE instantiateEitherName #-}
