-----------------------------------------------------------------------------
-- |
-- Module      :  Bound.Class
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Bound.Class
  ( Bound(..)
  , (=<<<)
  ) where

infixl 1 >>>=

-- | Instantces may or may not be monad transformers.
--
-- If they are, then you can use @m >>>= f = m >>= lift . f@
--
-- This is useful for types like expression lists, case alternatives,
-- schemas, etc. that may not be expressions in their own right, but often
-- contain one.

class Bound t where
  (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c
  -- default (>>>=) :: (MonadTrans t, Monad f) => t f a -> (a -> f c) -> t f c
  -- m >>>= f = m >>= lift . f

infixr 1 =<<<
(=<<<) :: (Bound t, Monad f) => (a -> f c) -> t f a -> t f c
(=<<<) = flip (>>>=)
