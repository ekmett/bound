module Simple where

-- this is a simple example where lambdas only bind a single variable at a time
-- this directly corresponds to the usual de bruijn presentation

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Prelude hiding (foldr)
import Prelude.Extras
import Bound

infixl 9 :@

data Exp a = Var a | Exp a :@ Exp a | Lam (Scope () Exp a)
  deriving (Eq,Ord,Show,Read)

lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

instance Eq1 Exp      where (==#)      = (==)
instance Ord1 Exp     where compare1   = compare
instance Show1 Exp    where showsPrec1 = showsPrec
instance Read1 Exp    where readsPrec1 = readsPrec
instance Functor Exp  where fmap       = fmapDefault
instance Foldable Exp where foldMap    = foldMapDefault

instance Applicative Exp where
  pure  = Var
  (<*>) = ap

instance Traversable Exp where
  traverse f (Var a)  = Var <$> f a
  traverse f (x :@ y) = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam e)  = Lam <$> traverse f e

instance Monad Exp where
  return         = Var
  Var a    >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)

-- \ x -> x
-- ghci> lam "x" (Var "x")
-- Lam (Var (Bound ()))

-- \ x -> x y
-- ghci> lam "x" (Var "x" :@ Var "y")
-- Lam (Var (Bound ()) :@ Var (Free (Var "y")))

-- \ y -> \x -> x y
-- ghci> lam "y" (lam "x" (Var "x" :@ Var "y"))
-- Lam (Lam (Var (Bound ()) :@ Var (Free (Var (Bound ())))))

