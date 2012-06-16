module Simple where

-- this is a simple example where lambdas only bind a single variable at a time
-- this directly corresponds to the usual de bruijn presentation

import Data.List (elemIndex)
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Traversable
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Prelude hiding (foldr,abs)
import Prelude.Extras
import Bound

infixl 9 :@

data Exp a
  = V a
  | Exp a :@ Exp a
  | Lam (Scope () Exp a)
  | Let [Scope Int Exp a] (Scope Int Exp a)
  deriving (Eq,Ord,Show,Read)

lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

let_ :: Eq a => [(a,Exp a)] -> Exp a -> Exp a
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where vs = map fst bs
        abstr = abstract (`elemIndex` vs)

instance Eq1 Exp      where (==#)      = (==)
instance Ord1 Exp     where compare1   = compare
instance Show1 Exp    where showsPrec1 = showsPrec
instance Read1 Exp    where readsPrec1 = readsPrec
instance Functor Exp  where fmap       = fmapDefault
instance Foldable Exp where foldMap    = foldMapDefault

instance Applicative Exp where
  pure  = V
  (<*>) = ap

instance Traversable Exp where
  traverse f (V a)    = V <$> f a
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam e)    = Lam <$> traverse f e
  traverse f (Let bs b) = Let <$> traverse (traverse f) bs <*> traverse f b

instance Monad Exp where
  return         = V
  V a    >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam e    >>= f = Lam (e >>>= f)
  Let bs b >>= f = Let (map (>>>= f) bs) (b >>>= f)

-- \ x -> x
-- ghci> lam "x" (V "x")
-- Lam (V (Bound ()))

-- \ x -> x y
-- ghci> lam "x" (V "x" :@ V "y")
-- Lam (V (Bound ()) :@ V (Free (V "y")))

-- \ y -> \x -> x y
-- ghci> lam "y" (lam "x" (V "x" :@ V "y"))
-- Lam (Lam (V (Bound ()) :@ V (Free (V (Bound ())))))

nf :: Exp a -> Exp a
nf e@V{}   = e
nf (Lam b) = Lam $ toScope $ nf $ fromScope b
-- nf (Lam (Scope b)) = Lam $ Scope $ fmap (fmap nf) (nf b)
nf (f :@ a) = case whnf f of
  Lam b -> nf (instantiate1 a b)
  f' -> nf f' :@ nf a
nf (Let bs b) = nf (inst b)
  where es = map inst bs
        inst = instantiate (es !!)

whnf :: Exp a -> Exp a
whnf e@V{}   = e
whnf e@Lam{} = e
whnf (f :@ a) = case whnf f of
  Lam b -> whnf (instantiate1 a b)
  f'    -> f' :@ a
whnf (Let bs b) = whnf (inst b)
  where es = map inst bs
        inst = instantiate (es !!)

cooked :: Exp a
cooked = fromJust $ closed $ let_
  [ ("False", lam "f" $ lam "t" $ V"f")
  , ("True" , lam "f" $ lam "t" $ V"t")
  , ("if"   , lam "b" $ lam "t" $ lam "f" $ V"b" :@ V"f" :@ V"t")
  , ("Zero" , lam "z" $ lam "s" $ V "z")
  , ("Succ" , lam "n" $ lam "z" $ lam "s" $ V"s" :@ V"n")
  , ("one"  , V"Succ" :@ V"Zero")
  , ("two"  , V"Succ" :@ V"one")
  , ("three", V"Succ" :@ V"two")
  , ("isZero", lam "n" $ V"n" :@ V"True" :@ (lam "m" $ V"False"))
  , ("const", lam "x" $ lam "y" $ V"x")
  , ("Pair", lam "a" $ lam "b" $ lam "p" $ V"p" :@ V"a" :@ V"b")
  , ("fst", lam "ab" $ V"ab" :@ (lam "a" $ lam "b" $ V"a"))
  , ("snd", lam "ab" $ V"ab" :@ (lam "a" $ lam "b" $ V"b"))
  , ("fix", lam "g" $ (lam "x" $ V"g":@ (V"x":@V"x")) :@ (lam "x" $ V"g":@ (V"x":@V"x")))
  , ("add", V"fix" :@ (lam "radd" $ lam "x" $ lam "y" $ V"x" :@ V"y" :@ (lam "n" $ V"Succ" :@ (V"radd" :@ V"n" :@ V"y"))))
  , ("mul", V"fix" :@ (lam "rmul" $ lam "x" $ lam "y" $ V"x" :@ V"Zero" :@ (lam "n" $ V"add" :@ V"y" :@ (V"rmul" :@ V"n" :@ V"y"))))
  , ("fac", V"fix" :@ (lam "rfac" $ lam "x" $ V"x" :@ V"one" :@ (lam "n" $ V"mul" :@ V"x" :@ (V"rfac" :@ V"n"))))
  , ("eqnat", V"fix" :@ (lam "reqnat" $ lam "x" $ lam "y" $ V"x" :@ (V"y" :@ V"True" :@ (V"const" :@ V"False")) :@ (lam "x1" $ V"y" :@ V"False" :@ (lam "y1" $ V"reqnat" :@ V"x1" :@ V"y1"))))
  , ("sumto", V"fix" :@ (lam "rsumto" $ lam "x" $ V"x" :@ V"Zero" :@ (lam "n" $ V"add" :@ V"x" :@ (V"rsumto" :@ V"n"))))
  , ("n5", V"add" :@ V"two" :@ V"three")
  , ("n6", V"add" :@ V"three" :@ V"three")
  , ("n17", V"add" :@ V"n6" :@ (V"add" :@ V"n6" :@ V"n5"))
  , ("n37", V"Succ" :@ (V"mul" :@ V"n6" :@ V"n6"))
  , ("n703", V"sumto" :@ V"n37")
  , ("n720", V"fac" :@ V"n6")
  , ("result", (V"eqnat" :@ V"n720" :@ (V"add" :@ V"n703" :@ V"n17")))
  ] (V "result")
