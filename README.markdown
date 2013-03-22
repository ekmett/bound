Bound
=====

[![Build Status](https://secure.travis-ci.org/ekmett/bound.png?branch=master)](http://travis-ci.org/ekmett/bound)

Goals
-----

This library provides convenient combinators for working with "locally-nameless" terms. These can be useful
when writing a type checker, evalator, parser, or pretty printer for terms that contain binders like forall
or lambda, as they ease the task of avoiding variable capture and testing for alpha-equivalence.

See [the documentation](http://hackage.haskell.org/package/bound) on hackage for more information, but here is an example:

     import Bound
     import Prelude.Extras

     infixl 9 :@
     data Exp a = V a | Exp a :@ Exp a | Lam (Scope () Exp a)
       deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

     instance Eq1 Exp
     instance Ord1 Exp
     instance Show1 Exp
     instance Read1 Exp
     instance Applicative Exp where pure = V; (<*>) = ap

     instance Monad Exp where
       return = V
       V a      >>= f = f a
       (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
       Lam e    >>= f = Lam (e >>>= f)

     lam :: Eq a => a -> Exp a -> Exp a
     lam v b = Lam (abstract1 v b)

     whnf :: Exp a -> Exp a
     whnf (f :@ a) = case whnf f of
       Lam b -> whnf (instantiate1 a b)
       f'    -> f' :@ a
     whnf e = e

   There are longer examples in the [examples/ folder](https://github.com/ekmett/bound/tree/master/examples).

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

