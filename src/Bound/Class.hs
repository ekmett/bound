{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__)
{-# LANGUAGE DefaultSignatures #-}
#endif
{-# OPTIONS -Wno-deprecations #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2015 Edward Kmett
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

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.Error
import Control.Monad.Trans.List
#endif

infixl 1 >>>=

-- | Instances of 'Bound' generate left modules over monads.
--
-- This means they should satisfy the following laws:
--
-- @
-- m '>>>=' 'return' ≡ m
-- m '>>>=' (λ x → k x '>>=' h) ≡ (m '>>>=' k) '>>>=' h
-- @
--
-- This guarantees that a typical Monad instance for an expression type
-- where Bound instances appear will satisfy the Monad laws (see doc/BoundLaws.hs).
--
-- If instances of 'Bound' are monad transformers, then @m '>>>=' f ≡ m '>>=' 'lift' '.' f@
-- implies the above laws, and is in fact the default definition.
--
-- This is useful for types like expression lists, case alternatives,
-- schemas, etc. that may not be expressions in their own right, but often
-- contain expressions.
--
-- /Note:/ 'Control.Monad.Free.Free' isn't "really" a monad transformer, even if
-- the kind matches. Therefore there isn't @'Bound' 'Control.Monad.Free.Free'@ instance.
class Bound t where
  -- | Perform substitution
  --
  -- If @t@ is an instance of @MonadTrans@ and you are compiling on GHC >= 7.4, then this
  -- gets the default definition:
  --
  -- @m '>>>=' f = m '>>=' 'lift' '.' f@
  (>>>=) :: Monad f => t f a -> (a -> f c) -> t f c
#if defined(__GLASGOW_HASKELL__)
  default (>>>=) :: (MonadTrans t, Monad f, Monad (t f)) =>
                    t f a -> (a -> f c) -> t f c
  m >>>= f = m >>= lift . f
  {-# INLINE (>>>=) #-}
#endif

instance Bound (ContT c) where
  m >>>= f = m >>= lift . f
  {-# INLINE (>>>=) #-}

instance Bound IdentityT where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

instance Bound MaybeT where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

instance Monoid w => Bound (RWST r w s) where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

instance Bound (ReaderT r) where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

instance Bound (StateT s) where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

instance Monoid w => Bound (WriterT w) where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

#if !(MIN_VERSION_transformers(0,6,0))
instance Error e => Bound (ErrorT e) where
 m >>>= f = m >>= lift . f
 {-# INLINE (>>>=) #-}

instance Bound ListT where
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
