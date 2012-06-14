-----------------------------------------------------------------------------
-- |
-- Module      :  Bound.Var
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Bound.Var (Var(..)) where

import Data.Foldable
import Data.Traversable
import Data.Monoid (mempty)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Applicative
import Control.Monad (ap)
import Prelude.Extras
import Text.Read

data Var b a
  = Bound b
  | Free a
  deriving (Eq,Ord,Show,Read)

instance Functor (Var b) where
  fmap _ (Bound b) = Bound b
  fmap f (Free a) = Free (f a)

instance Foldable (Var b) where
  foldMap f (Free a) = f a
  foldMap _ _ = mempty

instance Traversable (Var b) where
  traverse f (Free a) = Free <$> f a
  traverse _ (Bound b) = pure (Bound b)

instance Applicative (Var b) where
  pure = Free
  (<*>) = ap

instance Monad (Var b) where
  return = Free
  Free a  >>= f = f a
  Bound b >>= _ = Bound b

instance Bifunctor Var where
  bimap f _ (Bound b) = Bound (f b)
  bimap _ g (Free a) = Free (g a)

instance Bifoldable Var where
  bifoldMap f _ (Bound b) = f b
  bifoldMap _ g (Free a) = g a

instance Bitraversable Var where
  bitraverse f _ (Bound b) = Bound <$> f b
  bitraverse _ g (Free a) = Free <$> g a

instance Eq2 Var   where (==##)     = (==)
instance Ord2 Var  where compare2   = compare
instance Show2 Var where showsPrec2 = showsPrec
instance Read2 Var where readPrec2  = readPrec

instance Eq b   => Eq1   (Var b) where (==#)      = (==)
instance Ord b  => Ord1  (Var b) where compare1   = compare
instance Show b => Show1 (Var b) where showsPrec1 = showsPrec
instance Read b => Read1 (Var b) where readPrec1  = readPrec
