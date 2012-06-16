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
  , isClosed
  , closed
  ) where

import Data.Traversable
import Data.Maybe (isJust)

-- | @substitute p a w@ replaces the free variable @a@ with @p@ in @w@
substitute :: (Monad f, Eq a) => f a -> a -> f a -> f a
substitute p a w = w >>= \b -> if a == b then p else return b
{-# INLINE substitute #-}

-- | If a term has no free variables, you can freely change the type of free variables it uses
closed :: Traversable f => f a -> Maybe (f b)
closed = traverse (const Nothing)
{-# INLINE closed #-}

isClosed :: Traversable f => f a -> Bool
isClosed = isJust . closed
{-# INLINE isClosed #-}
