-----------------------------------------------------------------------------
-- |
-- Module      :  Bound.Term
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Bound.Term
  ( substitute
  , substituteVar
  , isClosed
  , closed
  ) where

import Data.Foldable
import Data.Traversable
import Prelude hiding (all)

-- | @'substitute' a p w@ replaces the free variable @a@ with @p@ in @w@.
--
-- >>> substitute "hello" ["goodnight","Gracie"] ["hello","!!!"]
-- ["goodnight","Gracie","!!!"]
substitute :: (Monad f, Eq a) => a -> f a -> f a -> f a
substitute a p w = w >>= \b -> if a == b then p else return b
{-# INLINE substitute #-}

-- | @'substituteVar' a b w@ replaces a free variable @a@ with another free variable @b@ in @w@.
--
-- >>> substitute "Alice" "Bob" ["Alice","Bob","Charlie"]
-- ["Bob","Bob","Charlie"]
substituteVar :: (Functor f, Eq a) => a -> a -> f a -> f a
substituteVar a p = fmap (\b -> if a == b then p else b)
{-# INLINE substituteVar #-}

-- | If a term has no free variables, you can freely change the type of
-- free variables it is parameterized on.
--
-- >>> closed [12]
-- Nothing
--
-- >>> closed ""
-- Just []
--
-- >>> :t closed ""
-- closed "" :: Maybe [b]
closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)
{-# INLINE closed #-}

-- | A closed term has no free variables.
--
-- >>> isClosed []
-- True
--
-- >>> isClosed [1,2,3]
-- False
isClosed :: Foldable f => f a -> Bool
isClosed = all (const False)
{-# INLINE isClosed #-}
