{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wincomplete-patterns -Wno-orphans #-}

module Main where

import Data.Kind
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.List as List
import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative
import Prelude hiding (foldr)
import Data.Functor.Classes
import Data.Type.Equality
import Bound

infixl 9 :@
infixr 5 :>

data Exp a
  = Var a
  | Exp a :@ Exp a
  | forall (b :: Index). Lam (Pat b Exp a) (Scope (Path b) Exp a)
  | Let (Vector (Scope Int Exp a)) (Scope Int Exp a)

data Index = VarI | WildI | AsI Index | ConI [Index]

data Pat :: Index -> (Type -> Type) -> Type -> Type where
  VarP  ::                             Pat 'VarI f a
  WildP ::                             Pat 'WildI f a
  AsP   :: Pat i f a                -> Pat ('AsI i) f a
  ConP  :: String    -> Pats bs f a -> Pat ('ConI bs) f a
  ViewP :: f a       -> Pat b f a   -> Pat b f a -- TODO: allow references to earlier variables

data Pats :: [Index] -> (Type -> Type) -> Type -> Type where
  NilP  :: Pats '[] f a
  (:>) :: Pat b f a -> Pats bs f a -> Pats (b ': bs) f a

data Path :: Index -> Type where
  V :: Path 'VarI
  L :: Path ('AsI a)
  R :: Path a -> Path ('AsI a)
  C :: MPath as -> Path ('ConI as)

data MPath :: [Index] -> Type where
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
#if !(MIN_VERSION_base(4,11,0))
  return         = Var
#endif
  Var a    >>= f = f a
  (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
  Lam p e  >>= f = Lam (p >>>= f) (e >>>= f)
  Let bs e >>= f = Let (fmap (>>>= f) bs) (e >>>= f)

instance Eq a => Eq (Exp a) where (==) = eq1
instance Eq1 Exp where
  liftEq eq (Var a)    (Var b)     = eq a b
  liftEq eq (a :@ a')  (b :@ b')   = liftEq eq a b && liftEq eq a' b'
  liftEq eq (Lam ps a) (Lam qs b)  =
    case eqPat' eq ps qs of
      Nothing -> False
      Just Refl -> liftEq eq a b

  liftEq eq (Let as a) (Let bs b)  = liftEq (liftEq eq) as bs && liftEq eq a b
  liftEq _  _          _           = False

instance Show a => Show (Exp a) where showsPrec = showsPrec1
instance Show1 Exp where
  liftShowsPrec s _ d (Var a)     = showParen (d > 10) $ showString "Var " . s 11 a
  liftShowsPrec s sl d (a :@ b)   = showParen (d > 9)  $ liftShowsPrec s sl 9 a . showString " :@ " . liftShowsPrec s sl 10 b
  liftShowsPrec s sl d (Lam ps b) = showParen (d > 10) $ showString "Lam " . liftShowsPrec s sl 11 ps . showChar ' ' . liftShowsPrec s sl 11 b
  liftShowsPrec s sl d (Let bs b) = showParen (d > 10) $ showString "Let " . liftShowsPrec (liftShowsPrec s sl) (liftShowList s sl) 11 bs . showChar ' ' . liftShowsPrec s sl 11 b

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
      Ps ps' ass g' -> Ps (p :> ps') (as ++ ass) $ \v ->
        T <$> g' v <|> H <$> f v

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

eqPat :: (Eq1 f) => (a -> b -> Bool) -> Pat i f a -> Pat i' f b -> Bool
eqPat _  VarP        VarP        = True
eqPat _  WildP       WildP       = True
eqPat eq (AsP p)     (AsP q)     = eqPat eq p q
eqPat eq (ConP g ps) (ConP h qs) = g == h  && eqPats eq ps qs
eqPat eq (ViewP e p) (ViewP f q) = liftEq eq e f && eqPat eq p q
eqPat _ _ _ = False

-- The same as eqPat, but if the patterns are equal, it returns a
-- proof that their type arguments are the same.
eqPat' :: (Eq1 f) => (a -> a' -> Bool) -> Pat b f a -> Pat b' f a' -> Maybe (b :~: b')
eqPat' _  VarP VarP = Just Refl
eqPat' _  WildP WildP = Just Refl
eqPat' eq (AsP p) (AsP q) = do
  Refl <- eqPat' eq p q
  Just Refl
eqPat' eq (ConP g ps) (ConP h qs) = do
  guard (g == h)
  Refl <- eqPats' eq ps qs
  Just Refl
eqPat' eq (ViewP e p) (ViewP f q) = guard (liftEq eq e f) >> eqPat' eq p q
eqPat' _ _ _ = Nothing

instance Eq1 f   => Eq1 (Pat b f)        where liftEq = eqPat
instance (Eq1 f, Eq a) => Eq (Pat b f a) where (==) = eq1

instance (Show1 f, Show a) => Show (Pat b f a) where showsPrec = showsPrec1

instance Show1 f => Show1 (Pat b f) where
  liftShowsPrec _ _  _ VarP        = showString "VarP"
  liftShowsPrec _ _  _ WildP       = showString "WildP"
  liftShowsPrec s sl d (AsP p)     = showParen (d > 10) $ showString "AsP " . liftShowsPrec s sl 11 p
  liftShowsPrec s sl d (ConP g ps) = showParen (d > 10) $ showString "ConP " . showsPrec 11 g . showChar ' ' . liftShowsPrec s sl 11 ps
  liftShowsPrec s sl d (ViewP e p) = showParen (d > 10) $ showString "ViewP " . liftShowsPrec s sl 11 e . showChar ' ' . liftShowsPrec s sl 11 p

instance Functor f => Functor (Pat b f) where
  fmap _ VarP = VarP
  fmap _ WildP = WildP
  fmap f (AsP p) = AsP (fmap f p)
  fmap f (ConP g ps) = ConP g (fmap f ps)
  fmap f (ViewP e p) = ViewP (fmap f e) (fmap f p)

instance Foldable f => Foldable (Pat b f) where
  foldMap f (AsP p)     = foldMap f p
  foldMap f (ConP _g ps) = foldMap f ps
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
eqPats :: (Eq1 f) => (a -> b -> Bool) -> Pats bs f a -> Pats bs' f b -> Bool
eqPats _  NilP      NilP      = True
eqPats eq (p :> ps) (q :> qs) = eqPat eq p q && eqPats eq ps qs
eqPats _  _         _         = False

-- Like eqPats, but if the patses are equal, it returns a proof that their
-- type arguments are the same.
eqPats' :: (Eq1 f) => (a -> a' -> Bool) -> Pats bs f a -> Pats bs' f a' -> Maybe (bs :~: bs')
eqPats' _  NilP NilP = Just Refl
eqPats' eq (p :> ps) (q :> qs) = do
  Refl <- eqPat' eq p q
  Refl <- eqPats' eq ps qs
  Just Refl
eqPats' _ _ _ = Nothing

instance Eq1 f         => Eq1 (Pats bs f)   where liftEq = eqPats
instance (Eq1 f, Eq a) => Eq  (Pats bs f a) where (==)  = eq1

instance (Show1 f, Show a) => Show (Pats bs f a) where showsPrec = showsPrec1
instance Show1 f => Show1 (Pats bs f) where
  liftShowsPrec _ _  _ NilP      = showString "NilP"
  liftShowsPrec s sl d (p :> ps) = showParen (d > 5) $
    liftShowsPrec s sl 6 p . showString " :> " . liftShowsPrec s sl 5 ps

instance Functor f => Functor (Pats bs f) where
  fmap _ NilP = NilP
  fmap f (p :> ps) = fmap f p :> fmap f ps

instance Foldable f => Foldable (Pats bs f) where
  foldMap f (p :> ps) = foldMap f p `mappend` foldMap f ps
  foldMap _ _    = mempty

instance Traversable f => Traversable (Pats bs f) where
  traverse _f NilP = pure NilP
  traverse f (p :> ps) = (:>) <$> traverse f p <*> traverse f ps

instance Bound (Pats bs) where
  NilP >>>= _ = NilP
  (p :> ps) >>>= f = (p >>>= f) :> (ps >>>= f)

-- ** Path into Pats
-- Internally, this is only used to implement eqPath, which is only
-- used to implement this.
eqMPath :: MPath is -> MPath js -> Bool
eqMPath (H m) (H n) = eqPath m n
eqMPath (T p) (T q) = eqMPath p q
eqMPath _     _     = False

instance Eq (MPath is) where
    H m == H n = m == n
    T p == T q = p == q
    _   == _   = False

-- Internally, this is only used to define comparePath, which
-- is only used here to define this.
compareMPath :: MPath is -> MPath js -> Ordering
compareMPath (H m) (H n) = comparePath m n
compareMPath (H _) (T _) = LT
compareMPath (T p) (T q) = compareMPath p q
compareMPath (T _) (H _) = GT

instance Ord (MPath is) where
    compare (H m) (H n) = compare m n
    compare (H _) (T _) = LT
    compare (T p) (T q) = compare p q
    compare (T _) (H _) = GT

instance Show (MPath is) where
  showsPrec d (H m) = showParen (d > 10) $ showString "H " . showsPrec 11 m
  showsPrec d (T p) = showParen (d > 10) $ showString "T " . showsPrec 11 p

-- instance Read (MPath is)

-- ** Path into Pat
-- Internally, this is only used to implement eqMPath, which is only used
-- to implement this.
eqPath :: Path i -> Path j -> Bool
eqPath V     V     = True
eqPath L     L     = True
eqPath (R m) (R n) = eqPath m n
eqPath (C p) (C q) = eqMPath p q
eqPath _     _     = False

instance Eq (Path i) where
    p == q = case compare p q of
               EQ -> True
               _ -> False

-- Internally, this is only used to define compareMPath, which
-- is only used here to define this.
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
    compare V y = case (y :: Path 'VarI) of V -> EQ
    compare L y = cpL y
        where
          cpL :: Path ('AsI a) -> Ordering
          cpL L = EQ
          cpL (R _) = LT
    compare (R r) y = cpR r y
        where
          cpR :: Path a -> Path ('AsI a) -> Ordering
          cpR _ L = GT
          cpR m (R n) = compare m n
    compare (C c) y = cpC c y
        where
          cpC :: MPath as -> Path ('ConI as) -> Ordering
          cpC p (C q) = compare p q

instance Show (Path i) where
  showsPrec _ V     = showString "V"
  showsPrec _ L     = showString "L"
  showsPrec d (R m) = showParen (d > 10) $ showString "R " . showsPrec 11 m
  showsPrec d (C p) = showParen (d > 10) $ showString "C " . showsPrec 11 p

-- |
-- >>> let_ [("x",Var "y"),("y",Var "x" :@ Var "y")] $ lam (varp "z") (Var "z" :@ Var "y")
-- Let (fromList [Scope (Var (B 1)),Scope (Var (B 0) :@ Var (B 1))]) (Scope (Lam VarP (Scope (Var (B V) :@ Var (F (Var (B 1)))))))
--
-- >>> lam (varp "x") (Var "x")
-- Lam VarP (Scope (Var (B V)))
--
-- >>> lam (conp "Hello" [varp "x", wildp]) (Var "y")
-- Lam (ConP "Hello" (VarP :> WildP :> NilP)) (Scope (Var (F (Var "y"))))
main :: IO ()
main = return ()
