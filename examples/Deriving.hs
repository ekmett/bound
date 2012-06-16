{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Exp where

import Data.List
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Prelude.Extras
import Bound

infixl 9 :@

data Exp a
  = Var a
  | Exp a :@ Exp a
  | Lam {-# UNPACK #-} !Int (Pat Exp a) (Scope Int Exp a)
  | Let {-# UNPACK #-} !Int [Scope Int Exp a] (Scope Int Exp a)
  | Case (Exp a) [Alt Exp a]
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return          = Var
  Var a      >>= f = f a
  (x :@ y)   >>= f = (x >>= f) :@ (y >>= f)
  Lam n p e  >>= f = Lam n (p >>>= f) (e >>>= f)
  Let n bs e >>= f = Let n (map (>>>= f) bs) (e >>>= f)
  Case e as  >>= f = Case (e >>= f) (map (>>>= f) as)

instance Eq1   Exp where (==#) = (==)
instance Ord1  Exp where compare1 = compare
instance Show1 Exp where showsPrec1 = showsPrec
instance Read1 Exp where readsPrec1 = readsPrec

data Pat f a
  = VarP
  | WildP
  | AsP (Pat f a)
  | ConP String [Pat f a]
  | ViewP (f a) (Pat f a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Bound Pat where
  VarP      >>>= _ = VarP
  WildP     >>>= _ = WildP
  AsP p     >>>= f = AsP (p >>>= f)
  ConP g ps >>>= f = ConP g (map (>>>= f) ps)
  ViewP e p >>>= f = ViewP (e >>= f) (p >>>= f)

data Alt f a = Alt {-# UNPACK #-} !Int (Pat f a) (Scope Int f a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Bound Alt where
  Alt n p b >>>= f = Alt n (p >>>= f) (b >>>= f)

-- ** smart patterns

data P a = P { pattern :: Pat Exp a, bindings :: [a] }

varp :: a -> P a
varp a = P VarP [a]

wildp :: P a
wildp = P WildP []

asp :: a -> P a -> P a
asp a (P p as) = P (AsP p) (a:as)

conp :: String -> [P a] -> P a
conp g ps = P (ConP g (map pattern ps)) (ps >>= bindings)

-- | smart lam constructor
lam :: Eq a => P a -> Exp a -> Exp a
lam (P p as) t = Lam (length as) p (abstract (`elemIndex` as) t)

-- | smart let constructor
let_ :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
let_ bs b = Let (length bs) (map (abstr . snd) bs) (abstr b)
  where vs  = map fst bs
        abstr = abstract (`elemIndex` vs)

-- | smart alt constructor
alt :: Eq a => P a -> Exp a -> Alt Exp a
alt (P p as) t = Alt (length as) p (abstract (`elemIndex` as) t)

-- ghci> let_ [("x",Var "y"),("y",Var "x" :@ Var "y")] $ lam (varp "z") (Var "z" :@ Var "y")
-- ghci> lam (varp "x") (Var "x")
-- ghci> lam (conp "Hello" [varp "x", wildp])) (Var "y")
-- ghci> lam (varp "x") $ Case (Var "x") [alt (conp "Hello" [varp "z",wildp]) (Var "x"), alt (varp "y") (Var "y")]
