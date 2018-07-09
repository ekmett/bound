{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE QuantifiedConstraints #-}
#endif
module Bound.ScopeT (
    ScopeT (..),
    (>>>>=),
    -- * Abstraction
    abstractT, abstract1T, abstractTEither,
    -- ** Name
    abstractTName, abstract1TName,
    -- * Instantiation
    instantiateT, instantiate1T, instantiateTEither,
    -- * Traditional de Bruijn
    fromScopeT,
    toScopeT,
    -- * Bound variable manipulation
    lowerScopeT,
    splatT,
    bindingsT,
    ) where

import Bound
import Bound.Name
import Data.Bifunctor
import Data.Functor.Classes

-- | @'Scope' b f a@ is a @t f@ expression abstracted over @f@,
-- with bound variables in @b@, and free variables in @a@.
--
-- @'Scope' n f a ~ 'ScopeT' n 'IdentityT' f a@
-- @'ScopeT' n t f a ~ t ('Scope' n f) a@
-- 
newtype ScopeT n t f a = ScopeT { unscopeT :: t f (Var n (f a)) }

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (Functor (t f), Functor f) => Functor (ScopeT n t f) where
   fmap f (ScopeT a) = ScopeT $ fmap (fmap (fmap f)) a

instance (Foldable (t f), Foldable f) => Foldable (ScopeT n t f) where
    foldMap f (ScopeT a) = foldMap (foldMap (foldMap f)) a
    foldr f z (ScopeT a) = foldr (flip (foldr (flip (foldr f))))  z a

instance (Traversable (t f), Traversable f) => Traversable (ScopeT n t f) where
    traverse f (ScopeT a) = ScopeT <$> traverse (traverse (traverse f)) a

-- | We cannot write @'Bound' ('ScopeT' n t)@ pre-GHC-8.6 (without an auxiliary type class).
(>>>>=) :: (Monad f, Functor (t f)) => ScopeT n t f a -> (a -> f b) -> ScopeT n t f b
ScopeT m >>>>= k = ScopeT $ fmap (fmap (>>= k)) m
{-# INLINE (>>>>=) #-}

#if __GLASGOW_HASKELL__ >= 805
-- | @(>>>=) :: ... => 'ScopeT' n t f a -> (a -> f b) -> 'ScopeT' n t f b@
instance (forall f. Functor f => Functor (t f)) => Bound (ScopeT n t) where
    (>>>=) = (>>>>=)
#endif

{-
instance (Hashable b, Monad f, Hashable1 f) => Hashable1 (Scope b f) where
  liftHashWithSalt h s m = liftHashWithSalt (liftHashWithSalt h) s (fromScope m)
  {-# INLINE liftHashWithSalt #-}

instance (Hashable b, Monad f, Hashable1 f, Hashable a) => Hashable (Scope b f a) where
  hashWithSalt n m = hashWithSalt1 n (fromScope m)
  {-# INLINE hashWithSalt #-}

instance NFData (f (Var b (f a))) => NFData (Scope b f a) where
  rnf scope = rnf (unscope scope)
-}

instance (Monad f, Bound t, Eq b, Eq1 (t f), Eq1 f, Eq a) => Eq  (ScopeT b t f a) where (==) = eq1
instance (Monad f, Bound t, Ord b, Ord1 (t f), Ord1 f, Ord a) => Ord  (ScopeT b t f a) where compare = compare1

#if MIN_VERSION_transformers(0,5,0) || !(MIN_VERSION_transformers(0,4,0))
-------------------------------------------------------------------------------
-- * transformers 0.5 Data.Functor.Classes
-------------------------------------------------------------------------------

instance (Show n, Show1 (t f), Show1 f, Show a) => Show (ScopeT n t f a) where
    showsPrec = showsPrec1

instance (Read n, Read1 (t f), Read1 f, Read a) => Read (ScopeT n t f a) where
    readsPrec = readsPrec1

instance (Monad f, Bound t, Eq b, Eq1 (t f), Eq1 f) => Eq1 (ScopeT b t f) where
  liftEq f m n = liftEq (liftEq f) (fromScopeT m) (fromScopeT n)

instance (Monad f, Bound t, Ord b, Ord1 (t f), Ord1 f) => Ord1 (ScopeT b t f) where
  liftCompare f m n = liftCompare (liftCompare f) (fromScopeT m) (fromScopeT n)

instance (Show n, Show1 (t f), Show1 f) => Show1 (ScopeT n t f) where
    liftShowsPrec sp sl d (ScopeT x) = showsUnaryWith
        (liftShowsPrec (liftShowsPrec sp' sl') (liftShowList sp' sl'))
        "ScopeT" d x
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Read n, Read1 (t f), Read1 f) => Read1 (ScopeT n t f) where
    liftReadsPrec f g = readsData $ readsUnaryWith
        (liftReadsPrec (liftReadsPrec f' g') (liftReadList f' g'))
        "ScopeT" ScopeT
      where
        f' = liftReadsPrec f g
        g' = liftReadList f g

#else

--------------------------------------------------------------------------------
-- * transformers 0.4 Data.Functor.Classes
--------------------------------------------------------------------------------

instance (Functor f, Functor (t f), Read b, Read1 (t f), Read1 f, Read a) => Read  (ScopeT b t f a) where readsPrec = readsPrec1
instance (Functor f, Functor (t f), Show b, Show1 (t f), Show1 f, Show a) => Show (ScopeT b t f a) where showsPrec = showsPrec1

instance (Monad f, Bound t, Eq b, Eq1 (t f), Eq1 f) => Eq1 (ScopeT b t f) where
  eq1 a b = eq1 (fromScopeT a) (fromScopeT b)

instance (Monad f, Bound t, Ord b, Ord1 (t f), Ord1 f) => Ord1 (ScopeT b t f) where
  compare1 a b = fromScopeT a `compare1` fromScopeT b

newtype Lift1 f a = Lift1 { lower1 :: f a }
instance (Show1 f, Show a) => Show (Lift1 f a) where showsPrec d (Lift1 m) = showsPrec1 d m
instance (Read1 f, Read a) => Read (Lift1 f a) where
    readsPrec d m = fmap (first Lift1) $ readsPrec1 d m

instance (Functor f, Functor (t f), Show b, Show1 (t f),Show1 f) => Show1 (ScopeT b t f) where
    showsPrec1 d a = showParen (d > 10) $
        showString "ScopeT " . showsPrec1 11 (fmap (fmap Lift1) (unscopeT a))

instance (Functor f, Functor (t f), Read b, Read1 (t f), Read1 f) => Read1 (ScopeT b t f) where
    readsPrec1 d = readParen (d > 10) $ \r -> do
        ("ScopeT", r') <- lex r
        (s, r'') <- readsPrec1 11 r'
        return (ScopeT (fmap (fmap lower1) s), r'')

#endif

-------------------------------------------------------------------------------
-- Abstraction
-------------------------------------------------------------------------------

abstractT :: (Functor (t f), Monad f) => (a -> Maybe n) -> t f a -> ScopeT n t f a
abstractT f e = ScopeT (fmap k e) where
    k y = case f y of
        Just z  -> B z
        Nothing -> F (return y)
{-# INLINE abstractT #-}

-- | Abstract over a single variable.
--
-- >>> abstract1T 'x' (MaybeT (Nothing : map Just "xyz"))
-- ScopeT (MaybeT [Nothing,Just (B ()),Just (F "y"),Just (F "z")])
abstract1T :: (Functor (t f), Monad f, Eq a) => a -> t f a -> ScopeT () t f a
abstract1T a = abstractT (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1T #-}

abstractTEither :: (Functor (t f),  Monad f) => (a -> Either b c) -> t f a -> ScopeT b t f c
abstractTEither f e = ScopeT (fmap k e) where
    k y = case f y of
        Left z -> B z
        Right y' -> F (return y')
{-# INLINE abstractTEither #-}

-------------------------------------------------------------------------------
-- Abstraction with Name
-------------------------------------------------------------------------------

-- | Abstraction, capturing named bound variables.
abstractTName :: (Functor (t f), Monad f) => (a -> Maybe b) -> t f a -> ScopeT (Name a b) t f a
abstractTName f t = ScopeT (fmap k t) where
    k a = case f a of
        Just b  -> B (Name a b)
        Nothing -> F (return a)
{-# INLINE abstractTName #-}

-- | Abstract over a single variable
abstract1TName :: (Functor (t f), Monad f, Eq a) => a -> t f a -> ScopeT (Name a ()) t f a
abstract1TName a = abstractTName (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1TName #-}

-------------------------------------------------------------------------------
-- Instantiation
-------------------------------------------------------------------------------

-- | Enter a 'ScopeT', instantiating all bound variables
instantiateT :: (Bound t, Monad f) => (n -> f a) -> ScopeT n t f a -> t f a
instantiateT k (ScopeT e) = e >>>= \v -> case v of
    B b -> k b
    F a -> a
{-# INLINE instantiateT #-}

-- | Enter a 'ScopeT' that binds one variable, instantiating it
instantiate1T :: (Bound t, Monad f) => f a -> ScopeT n t f a -> t f a
instantiate1T e = instantiateT (const e)
{-# INLINE instantiate1T #-}

-- | Enter a 'ScopeT', and instantiate all bound and free variables in one go.
instantiateTEither :: (Bound t, Monad f) => (Either b a -> f c) -> ScopeT b t f a -> t f c
instantiateTEither f (ScopeT e) = e >>>= \v -> case v of
    B b -> f (Left b)
    F ea -> ea >>= f . Right
{-# INLINE instantiateTEither #-}

-------------------------------------------------------------------------------
-- Traditional de Bruijn
-------------------------------------------------------------------------------

-- | Convert to traditional de Bruijn.
fromScopeT :: (Bound t, Monad f) => ScopeT n t f a -> t f (Var n a)
fromScopeT (ScopeT s) = s >>>= \v -> case v of
    F e -> fmap F e
    B b -> return (B b)

-- | Convert from traditional de Bruijn to generalized de Bruijn indices.
toScopeT :: (Functor (t f), Monad f) => t f (Var n a) -> ScopeT n t f a
toScopeT e = ScopeT (fmap (fmap return) e)

-- | Convert to 'Scope'.
lowerScopeT
    :: (Functor (t f), Functor f)
    => (forall x. t f x -> g x)
    -> (forall x. f x -> g x)
    -> ScopeT n t f a -> Scope n g a
lowerScopeT tf f (ScopeT x) = Scope (tf (fmap (fmap f) x))

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- | Perform substitution on both bound and free variables in a 'ScopeT'.
splatT :: (Bound t, Monad f) => (a -> f c) -> (b -> f c) -> ScopeT b t f a -> t f c
splatT f unbind (ScopeT e) = e >>>= \v -> case v of
    B b -> unbind b
    F ea -> ea >>= f
{-# INLINE splatT #-}

-- | Return a list of occurences of the variables bound by this 'ScopeT'.
bindingsT :: Foldable (t f) => ScopeT b t f a -> [b]
bindingsT (ScopeT s) = foldr f [] s where
  f (B v) vs = v : vs
  f _ vs     = vs
{-# INLINE bindingsT #-}

-- $setup
-- >>> import Control.Monad.Trans.Maybe
