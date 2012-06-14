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
  ) where

infixl 1 >>>=
class Bound t where
  (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c

infixr 1 =<<<
(=<<<) :: (Bound t, Monad f) => (a -> f c) -> t f a -> t f c
(=<<<) = flip (>>>=)
