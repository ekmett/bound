{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module Exp where


import Data.Vector as Vector hiding ((++), map)
import Data.List as List
import Data.Foldable
import Data.Traversable
import Data.Monoid (Monoid(..))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Prelude hiding (foldr)
import Prelude.Extras
import GHC.Prim (Constraint(..))
import Unsafe.Coerce
import Bound

-- ghci> let_ [("x",Var "y"),("y",Var "x" :@ Var "y")] $ lam (varp "z") (Var "z" :@ Var "y")
-- Let (fromList [Scope (Var (B 1)),Scope (Var (B 0) :@ Var (B 1))]) (Scope (Lam VarP (Scope (Var (B V) :@ Var (F (Var (B 1)))))))
--
-- ghc> lam (varp "x") (Var "x")
-- ghc> lam (conp "Hello" [varp "x", wildp])) (Var "y")

infixl 9 :@
infixr 5 :>

-- little orphan instances
instance Show1 Vector where showsPrec1 = showsPrec
instance Eq1 Vector where (==#) = (==)

data Exp a
  = Var a
  | Exp a :@ Exp a
  | forall (b :: Index). Lam (Pat b Exp a) (Scope (Path b) Exp a)
  | Let (Vector (Scope Int Exp a)) (Scope Int Exp a)
  -- | Case (Exp a) [Alt Exp a]

data Alt f a = forall b. Alt (Pat b f a) (Scope (Path b) Exp a)

data Index = VarI | WildI | AsI Index | ConI [Index]

data Pat :: Index -> (* -> *) -> * -> * where
  VarP  ::                             Pat VarI f a
  WildP ::                             Pat WildI f a
  AsP   :: Pat i f a                -> Pat (AsI i) f a
  ConP  :: String    -> Pats bs f a -> Pat (ConI bs) f a
  ViewP :: f a       -> Pat b f a   -> Pat b f a -- TODO: allow references to earlier variables

data Pats :: [Index] -> (* -> *) -> * -> * where
  NilP  :: Pats '[] f a
  (:>) :: Pat b f a -> Pats bs f a -> Pats (b ': bs) f a

data Path :: Index -> * where
  V :: Path VarI
  L :: Path (AsI a)
  R :: Path a -> Path (AsI a)
  C :: MPath as -> Path (ConI as)

data MPath :: [Index] -> * where
  H :: Path a   -> MPath (a ':as)
  T :: MPath as -> MPath (a ':as)

instance Functor Exp where
  fmap = fmapDefault

instance Foldable Exp where
  foldMap = foldMapDefault

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Traversable Exp where
  traverse f (Var a)    = Var <$> f a
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam p e)  = Lam <$> traverse f p <*> traverse f e
  traverse f (Let bs e) = Let <$> traverse (traverse f) bs <*> traverse f e

instance Monad Exp where
  return         = Var
  Var a    >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam p e  >>= f = Lam (p >>>= f) (e >>>= f)
  Let bs e >>= f = Let (fmap (>>>= f) bs) (e >>>= f)
 -- Case e as >>= f = Case (e >>= f) (fmap (>>>= f) as)

instance Eq a => Eq (Exp a) where (==) = (==#)
instance Eq1 Exp where
  Var a     ==# Var b     = a == b
  (a :@ b)  ==# (c :@ d)  = a ==# c && b ==# d
  Lam ps a  ==# Lam qs b  = eqPat ps qs && a ==# unsafeCoerce b -- eqPat proves equal shape
  Let as a  ==# Let bs b  = as == bs && a ==# b
 -- Case e as ==# Case f bs = e ==# f && as == bs
  _         ==# _         = False

instance Show a => Show (Exp a) where showsPrec = showsPrec1
instance Show1 Exp where
  showsPrec1 d (Var a)    = showParen (d > 10) $ showString "Var " . showsPrec 11 a
  showsPrec1 d (a :@ b)   = showParen (d > 9) $ showsPrec1 9 a . showString " :@ " . showsPrec1 10 b
  showsPrec1 d (Lam ps b) = showParen (d > 10) $ showString "Lam " . showsPrec1 11 ps . showChar ' ' . showsPrec1 11 b
  showsPrec1 d (Let bs b) = showParen (d > 10) $ showString "Let " . showsPrec1 11 bs . showChar ' ' . showsPrec1 11 b

{-
instance Eq1 f => Eq1 (Alt f) where
  Alt p s ==# Alt q t = eqPat p q && s == unsafeCoerce t

instance (Eq1 f, Eq a) => Eq (Alt f) where (==) = (==#)

instance Show1 f => Show1 (Alt f) where
  showsPrec d (Alt p s) = showsPrec d (Alt p s)
-}


-- * smart lam

-- ** smart patterns

data P a = forall b. P (Pat b Exp a) [a] (a -> Maybe (Path b))

varp :: Eq a => a -> P a
varp a = P VarP [a] (\v -> if a == v then Just V else Nothing)

wildp :: P a
wildp = P WildP [] (const Nothing)

asp :: Eq a => a -> P a -> P a
asp a (P p as f) = P (AsP p) (a:as) $ \v -> case f v of
  Just b              -> Just (R b)
  Nothing | a == v    -> Just L
          | otherwise -> Nothing

data Ps a = forall bs. Ps (Pats bs Exp a) [a] (a -> Maybe (MPath bs))

conp :: String -> [P a] -> P a
conp g ps = case go ps of
  Ps qs as f -> P (ConP g qs) as (fmap C . f)
  where
    go :: [P a] -> Ps a
    go [] = Ps NilP [] (const Nothing)
    go (P p as f : xs) = case go xs of
      Ps ps ass g -> Ps (p :> ps) (as ++ ass) $ \v ->
        T <$> g v <|> H <$> f v

-- * smart lam
lam :: P a -> Exp a -> Exp a
lam (P p _ f) t = Lam p (abstract f t)

-- * smart let
let_ :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
let_ bs b = Let (Vector.fromList $ map (abstr . snd) bs) (abstr b)
  where vs  = map fst bs
        abstr = abstract (`List.elemIndex` vs)

-- * Pat

-- ** A Kind of Shape

eqPat :: (Eq1 f, Eq a) => Pat b f a -> Pat b' f a -> Bool
eqPat VarP        VarP        = True
eqPat WildP       WildP       = True
eqPat (AsP p)     (AsP q)     = eqPat p q
eqPat (ConP g ps) (ConP h qs) = g == h  && eqPats ps qs
eqPat (ViewP e p) (ViewP f q) = e ==# f && eqPat p q

instance Eq1 f   => Eq1 (Pat b f)        where (==#) = eqPat
instance (Eq1 f, Eq a) => Eq (Pat b f a) where (==) = eqPat

instance Show1 f => Show1 (Pat b f) where showsPrec1 = showsPrec
instance (Show1 f, Show a) => Show (Pat b f a) where
  showsPrec _ VarP        = showString "VarP"
  showsPrec _ WildP       = showString "WildP"
  showsPrec d (AsP p)     = showParen (d > 10) $ showString "AsP " . showsPrec 11 p
  showsPrec d (ConP g ps) = showParen (d > 10) $ showString "ConP " . showsPrec 11 g . showChar ' ' . showsPrec 11 ps
  showsPrec d (ViewP e p) = showParen (d > 10) $ showString "ViewP " . showsPrec1 11 e . showChar ' ' . showsPrec 11 p

instance Functor f => Functor (Pat b f) where
  fmap _ VarP = VarP
  fmap _ WildP = WildP
  fmap f (AsP p) = AsP (fmap f p)
  fmap f (ConP g ps) = ConP g (fmap f ps)
  fmap f (ViewP e p) = ViewP (fmap f e) (fmap f p)

instance Foldable f => Foldable (Pat b f) where
  foldMap f (AsP p)     = foldMap f p
  foldMap f (ConP g ps) = foldMap f ps
  foldMap f (ViewP e p) = foldMap f e `mappend` foldMap f p
  foldMap _ _           = mempty

instance Traversable f => Traversable (Pat b f) where
  traverse _ VarP = pure VarP
  traverse _ WildP = pure WildP
  traverse f (AsP p) = AsP <$> traverse f p
  traverse f (ConP g ps) = ConP g <$> traverse f ps
  traverse f (ViewP e p) = ViewP <$> traverse f e <*> traverse f p

instance Bound (Pat b) where
  VarP      >>>= _ = VarP
  WildP     >>>= _ = WildP
  AsP p     >>>= f = AsP (p >>>= f)
  ConP g ps >>>= f = ConP g (ps >>>= f)
  ViewP e p >>>= f = ViewP (e >>= f) (p >>>= f)

-- ** Pats


eqPats :: (Eq1 f, Eq a) => Pats bs f a -> Pats bs' f a -> Bool
eqPats NilP      NilP      = True
eqPats (p :> ps) (q :> qs) = eqPat p q && eqPats ps qs
eqPats _         _         = False

instance Eq1 f         => Eq1 (Pats bs f)   where (==#) = eqPats
instance (Eq1 f, Eq a) => Eq  (Pats bs f a) where (==)  = eqPats

instance (Show1 f, Show a) => Show (Pats bs f a) where showsPrec = showsPrec1
instance Show1 f => Show1 (Pats bs f) where
  showsPrec1 _ NilP      = showString "NilP"
  showsPrec1 d (p :> ps) = showParen (d > 5) $
    showsPrec1 6 p . showString " :> " . showsPrec1 5 ps

instance Functor f => Functor (Pats bs f) where
  fmap _ NilP = NilP
  fmap f (p :> ps) = fmap f p :> fmap f ps

instance Foldable f => Foldable (Pats bs f) where
  foldMap f (p :> ps) = foldMap f p `mappend` foldMap f ps
  foldMap _ _    = mempty

instance Traversable f => Traversable (Pats bs f) where
  traverse f NilP = pure NilP
  traverse f (p :> ps) = (:>) <$> traverse f p <*> traverse f ps

instance Bound (Pats bs) where
  NilP >>>= _ = NilP
  (p :> ps) >>>= f = (p >>>= f) :> (ps >>>= f)


-- ** Path into Pats

eqMPath :: MPath is -> MPath js -> Bool
eqMPath (H m) (H n) = eqPath m n
eqMPath (T p) (T q) = eqMPath p q
eqMPath _     _     = False
instance Eq (MPath is) where (==) = eqMPath

compareMPath :: MPath is -> MPath js -> Ordering
compareMPath (H m) (H n) = comparePath m n
compareMPath (H _) (T _) = LT
compareMPath (T p) (T q) = compareMPath p q
compareMPath (T _) (H _) = GT
instance Ord (MPath is) where compare = compareMPath

instance Show (MPath is) where
  showsPrec d (H m) = showParen (d > 10) $ showString "H " . showsPrec 11 m
  showsPrec d (T p) = showParen (d > 10) $ showString "T " . showsPrec 11 p

-- instance Read (MPath is)

-- ** Path into Pat


eqPath :: Path i -> Path j -> Bool
eqPath V     V     = True
eqPath L     L     = True
eqPath (R m) (R n) = eqPath m n
eqPath (C p) (C q) = eqMPath p q
eqPath _     _     = False

instance Eq (Path i) where (==) = eqPath

comparePath :: Path i -> Path j -> Ordering
comparePath V     V     = EQ
comparePath V     _     = LT
comparePath L     V     = GT
comparePath L     L     = EQ
comparePath L     _     = LT
comparePath (R _) V     = GT
comparePath (R _) L     = GT
comparePath (R m) (R n) = comparePath m n
comparePath (R _) (C _) = LT
comparePath (C p) (C q) = compareMPath p q
comparePath (C _) _     = GT

instance Ord (Path i) where
  compare V     V     = EQ
  compare L     L     = EQ
  compare L     _     = LT
  compare (R _) L     = GT
  compare (R m) (R n) = compare m n
  compare (C p) (C q) = compare p q

instance Show (Path i) where
  showsPrec _ V     = showString "V"
  showsPrec _ L     = showString "L"
  showsPrec d (R m) = showParen (d > 10) $ showString "R " . showsPrec 11 m
  showsPrec d (C p) = showParen (d > 10) $ showString "C " . showsPrec 11 p


