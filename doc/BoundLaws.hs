{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
module BoundLaws where

import Bound.Class
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Control.Monad

{-

What laws should Bound have?

We need at least enough to make sure the typical Monad Exp instances are valid.

Let's start by writing some generic Bound instances.

-}

newtype Const x (m :: * -> *) a = Const x

instance Bound (Const x) where
  Const x >>>= _ = Const x


newtype Identity (m :: * -> *) a = Id (m a)

instance Bound Identity where
   Id ma >>>= f = Id (ma >>= f)


data Product f g (m :: * -> *) a = f m a :*: g m a

instance (Bound f, Bound g) => Bound (Product f g) where
    (fma :*: gma) >>>= f = (fma >>>= f) :*: (gma >>>= f)


data Sum f g (m :: * -> *) a = Inl (f m a) | Inr (g m a)

instance (Bound f, Bound g) => Bound (Sum f g) where
    Inl fma >>>= f = Inl (fma >>>= f)
    Inr gma >>>= f = Inr (gma >>>= f)


{-

Now we can actually write the typical Monad Exp instance generically
(for theory, not practice), since sums and products and all of the
above is plenty enough to specify an AST.

-}

data Exp (f :: (* -> *) -> * -> *) a = Var a | Branch (f (Exp f) a)

instance Bound f => Functor (Exp f) where
  fmap = liftM

instance Bound f => Applicative (Exp f) where
  pure = Var
  (<*>) = ap

instance Bound f => Monad (Exp f) where
  return = Var
  Var a     >>= f = f a
  Branch fE >>= f = Branch (fE >>>= f)

{-

Is this valid? Let's go to Agda and try to prove the Monad laws.


  left-return : ∀ {A B} (x : A)(f : A -> Exp F B) -> (return x >>= f) ≡ f x
  left-return x f = refl

  right-return : ∀ {A}(m : Exp F A) -> (m >>= return) ≡ m
  right-return (Var x)    = refl
  right-return (Branch m) = cong Branch {!!}0

  assoc : ∀ {A B C} (m : Exp F A) (k : A -> Exp F B) (h : B -> Exp F C) -> (m >>= (\ x -> k x >>= h)) ≡ ((m >>= k) >>= h)
  assoc (Var x)    k h = refl
  assoc (Branch m) k h = cong Branch {!!}1


So the first one is fine, but we have two holes:

  ?0 : m >>>= return ≡ m
  ?1 : m >>>= (λ x → k x >>= h) ≡ (m >>>= k) >>>= h

But all of the instances above respect these laws, and they are implied by
the current law for monad transformers, we could just make them the
Bound class laws.

Btw these laws correspond to requiring (f m) to be an m-left module for every m [1],
so we'd also get a law-abiding fmap for (f m).


Bonus: composing pointwise (\m a -> f m (g m a)) would also create a valid Bound


[1] Modules over Monads and Initial Semantics - http://web.math.unifi.it/users/maggesi/syn.pdf
-}
