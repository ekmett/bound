{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DefaultSignatures #-}
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
-- This module provides the 'Bound' class, for performing substitution into
-- things that are not necessarily full monad transformers.
----------------------------------------------------------------------------
module Bound.Class
  ( Bound(..)
  , (=<<<)
  ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
import Control.Monad.Trans.Class
#endif

infixl 1 >>>=

-- | Instances of 'Bound' generate left modules over monads.
--
-- This means they should satisfy the following laws:
--
-- > m >>>= return ≡ m
-- > m >>>= (λ x → k x >>= h) ≡ (m >>>= k) >>>= h
--
-- This guarantees that a typical Monad instance for an expression type
-- where Bound instances appear will satisfy the Monad laws (see doc/BoundLaws.hs).
--
-- If instances of Bound are monad transformers, then @m '>>>=' f ≡ m '>>=' 'lift' '.' f@
-- implies the above laws, and is in fact the default definition.
--
-- This is useful for types like expression lists, case alternatives,
-- schemas, etc. that may not be expressions in their own right, but often
-- contain expressions.
class Bound t where
  -- | Perform substitution
  --
  -- If @t@ is an instance of @MonadTrans@ and you are compiling on GHC >= 7.4, then this
  -- gets the default definition:
  --
  -- @m '>>>=' f = m '>>=' 'lift' '.' f@
  (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
  default (>>>=) :: (MonadTrans t, Monad f, Monad (t f)) =>
                    t f a -> (a -> f c) -> t f c
  m >>>= f = m >>= lift . f
  {-# INLINE (>>>=) #-}
#endif

infixr 1 =<<<
-- | A flipped version of ('>>>=').
--
-- @('=<<<') = 'flip' ('>>>=')@
(=<<<) :: (Bound t, Monad f) => (a -> f c) -> t f a -> t f c
(=<<<) = flip (>>>=)
{-# INLINE (=<<<) #-}
