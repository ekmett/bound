{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards   #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This is a Template Haskell module for deriving 'Applicative' and
-- 'Monad' instances for data types.
----------------------------------------------------------------------------

module Bound.TH
  (
#ifdef MIN_VERSION_template_haskell
    makeBound
#endif
  ) where

#ifdef MIN_VERSION_template_haskell
import Data.List        (intercalate)
import Data.Traversable (for)
import Control.Monad    (foldM, mzero, guard)
import Bound.Class      (Bound((>>>=)))
import Language.Haskell.TH
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative, pure, (<*>))
#endif

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))

-- |
-- Use to automatically derive 'Applicative' and 'Monad' instances for
-- your datatype.
--
-- Also works for components that are lists or instances of 'Functor',
-- but still does not work for a great deal of other things.
--
-- @deriving-compat@ package may be used to derive the 'Show1' and 'Read1' instances
--
-- @
-- {-\# LANGUAGE DeriveFunctor      #-}
-- {-\# LANGUAGE TemplateHaskell    #-}
--
-- import Bound                (Scope, makeBound)
-- import Data.Functor.Classes (Show1, Read1, showsPrec1, readsPrec1)
-- import Data.Deriving        (deriveShow1, deriveRead1)
--
-- data Exp a
--   = V a
--   | App (Exp a) (Exp a)
--   | Lam (Scope () Exp a)
--   | ND [Exp a]
--   | I Int
--   deriving (Functor)
--
-- makeBound ''Exp
-- deriveShow1 ''Exp
-- deriveRead1 ''Exp
-- instance Read a => Read (Exp a) where readsPrec = readsPrec1
-- instance Show a => Show (Exp a) where showsPrec = showsPrec1
-- @
--
-- and in GHCi
--
-- @
-- ghci> :set -XDeriveFunctor
-- ghci> :set -XTemplateHaskell
-- ghci> import Bound                (Scope, makeBound)
-- ghci> import Data.Functor.Classes (Show1, Read1, showsPrec1, readsPrec1)
-- ghci> import Data.Deriving        (deriveShow1, deriveRead1)
-- ghci> :{
-- ghci| data Exp a = V a | App (Exp a) (Exp a) | Lam (Scope () Exp a) | ND [Exp a] | I Int deriving (Functor)
-- ghci| makeBound ''Exp
-- ghci| deriveShow1 ''Exp
-- ghci| deriveRead1 ''Exp
-- ghci| instance Read a => Read (Exp a) where readsPrec = readsPrec1
-- ghci| instance Show a => Show (Exp a) where showsPrec = showsPrec1
-- ghci| :}
-- @
--
-- 'Eq' and 'Ord' instances can be derived similarly
--
-- @
-- import Data.Functor.Classes (Eq1, Ord1, eq1, compare1)
-- import Data.Deriving        (deriveEq1, deriveOrd1)
--
-- deriveEq1 ''Exp
-- deriveOrd1 ''Exp
-- instance Eq a => Eq (Exp a) where (==) = eq1
-- instance Ord a => Ord (Exp a) where compare = compare1
-- @
--
-- or in GHCi:
--
-- @
-- ghci> import Data.Functor.Classes (Eq1, Ord1, eq1, compare1)
-- ghci> import Data.Deriving        (deriveEq1, deriveOrd1)
-- ghci> :{
-- ghci| deriveEq1 ''Exp
-- ghci| deriveOrd1 ''Exp
-- ghci| instance Eq a => Eq (Exp a) where (==) = eq1
-- ghci| instance Ord a => Ord (Exp a) where compare = compare1
-- ghci| :}
-- @
--
-- We cannot automatically derive 'Eq' and 'Ord' using the standard GHC mechanism,
-- because instances require @Exp@ to be a 'Monad':
--
-- @
-- instance (Monad f, Eq b, Eq1 f, Eq a)    => Eq (Scope b f a)
-- instance (Monad f, Ord b, Ord1 f, Ord a) => Ord (Scope b f a)
-- @

makeBound :: Name -> DecsQ
makeBound name = do
  TyConI dec <- reify name
  case dec of
#if MIN_VERSION_template_haskell(2,11,0)
    DataD _ _name vars _ cons _ -> makeBound' name vars cons
#else
    DataD _ _name vars cons _   -> makeBound' name vars cons
#endif
    _ -> fail $ show name ++ " Must be a data type."

makeBound' :: Name -> [TyVarBndr] -> [Con] -> DecsQ
makeBound' name vars cons = do
  let instanceHead :: Type
      instanceHead = name `conAppsT` map VarT (typeVars (init vars))

      var  :: ExpQ
      var  = ConE `fmap` getPure name vars cons

      bind :: ExpQ
      bind = constructBind name vars cons

#if __GLASGOW_HASKELL__ < 708
      def :: Name -> DecQ -> [DecQ]
#if __GLASGOW_HASKELL__ < 706
      def _theName dec = [dec]
#else
      def theName  dec = [pragInlD theName Inline FunLike AllPhases, dec]
#endif

      pureBody :: Name -> [DecQ]
      pureBody pure'or'return =
        def pure'or'return
          (valD (varP pure'or'return) (normalB var) [])

      bindBody :: [DecQ]
      bindBody =
        def '(>>=)
          (valD (varP '(>>=)) (normalB bind) [])

  apBody <- do
    ff <- newName "ff"
    fy <- newName "fy"
    f  <- newName "f"
    y  <- newName "y"

    -- \ff fy -> do
    --   f <- ff
    --   y <- fy
    --   pure (f x)
    let ap :: ExpQ
        ap = lamE [varP ff, varP fy] (doE
              [bindS   (varP f) (varE ff),
               bindS   (varP y) (varE fy),
               noBindS (varE 'pure `appE` (varE f `appE` varE y))])

    pure (def '(<*>) (valD (varP '(<*>)) (normalB ap) []))

  -- instance Applicative $name where
  --   pure   = $var
  --   (<*>)  = \ff fy -> do
  --     f <- ff
  --     y <- fy
  --     pure (f y)
  applicative <-
    instanceD (cxt []) (appT (conT ''Applicative) (pure instanceHead))
      (pureBody 'pure ++ apBody)

  -- instance Monad $name where
  --   return = $var
  --   (>>=)  = $bind
  monad <-
    instanceD (cxt []) (appT (conT ''Monad) (pure instanceHead))
      (pureBody 'return ++ bindBody)

  pure [applicative, monad]
#else
  [d| instance Applicative $(pure instanceHead) where
        pure = $var
        {-# INLINE pure #-}

        ff <*> fy = do
          f <- ff
          y <- fy
          pure (f y)
        {-# INLINE (<*>) #-}

      instance Monad $(pure instanceHead) where
# if __GLASGOW_HASKELL__ < 710
        return = $var
        {-# INLINE return #-}
# endif

        (>>=)  = $bind
        {-# INLINE (>>=) #-}
    |]
#endif

-- Internals
data Prop
  = Bound
  | Konst
  | Funktor Int -- ^ number tells how many layers are there
  | Exp
  deriving Show

data Components
  = Component Name [(Name, Prop)]
  | Variable Name
  deriving Show

constructBind :: Name -> [TyVarBndr] -> [Con] -> ExpQ
constructBind name vars cons = do
  interpret =<< construct name vars cons

construct :: Name -> [TyVarBndr] -> [Con] -> Q [Components]
construct name vars constructors = do
  var <- getPure name vars constructors
  for constructors $ \con -> do
    case con of
      NormalC conName [(_, _)]
        | conName == var
        -> pure (Variable conName)
      NormalC conName types
        -> Component conName `fmap` mapM typeToBnd [ ty | (_, ty) <- types ]
      RecC conName types
        -> Component conName `fmap` mapM typeToBnd [ ty | (_, _, ty) <- types ]
      InfixC (_, a) conName (_, b)
        -> do
        bndA <- typeToBnd a
        bndB <- typeToBnd b
        pure (Component conName [bndA, bndB])
      _ -> error "Not implemented."

  where
  expa :: Type
  expa = name `conAppsT` map VarT (typeVars vars)

  typeToBnd :: Type -> Q (Name, Prop)
  typeToBnd ty = do
    boundInstance <- isBound ty
    functorApp <- isFunctorApp ty
    var <- newName "var"
    pure $ case () of
      _ | ty == expa           -> (var, Exp)
        | boundInstance        -> (var, Bound)
        | isKonst ty           -> (var, Konst)
        | Just n <- functorApp -> (var, Funktor n)
        | otherwise            -> error $ "This is bad: "
                                        ++ show ty
                                        ++ " "
                                        ++ show boundInstance

  -- Checks whether a type is an instance of Bound by stripping its last
  -- two type arguments:
  --     isBound (Scope () EXP a)
  --  -> isInstance ''Bound [Scope ()]
  --  -> True
  isBound :: Type -> Q Bool
  isBound ty
    -- We might fail with kind error, but we don't care
    | Just a <- stripLast2 ty = pure False `recover` isInstance ''Bound [a]
    | otherwise               = return False

  isKonst :: Type -> Bool
  isKonst ConT {} = True
  isKonst (VarT n) = n /= varBindName (last vars)
  isKonst (AppT a b) = isKonst a && isKonst b
  isKonst _ = False

  isFunctorApp :: Type -> Q (Maybe Int)
  isFunctorApp = runMaybeT . go
    where
      go x | x == expa  = pure 0
      go (f `AppT` x)   = do
          isFunctor <- lift $ isInstance ''Functor [f]
          guard isFunctor
          n <- go x
          pure $ n + 1
      go _              = mzero

interpret :: [Components] -> ExpQ
interpret bnds = do
  x       <- newName "x"
  f       <- newName "f"

  let
    bind :: Components -> MatchQ
    bind (Variable name) = do
      a <- newName "a"
      match
        (conP name [varP a])
        (normalB (varE f `appE` varE a))
        []

    bind (Component name bounds) = do
     exprs <- foldM bindOne (ConE name) bounds
     pure $
       Match
       (ConP name [ VarP arg | (arg, _) <- bounds ])
       (NormalB
         exprs)
        []

    bindOne :: Exp -> (Name, Prop) -> Q Exp
    bindOne expr (name, bnd) = case bnd of
      Bound ->
        pure expr `appE` (varE '(>>>=) `appE` varE name `appE` varE f)
      Konst ->
        pure expr `appE` varE name
      Exp   ->
        pure expr `appE` (varE '(>>=) `appE` varE name `appE` varE f)
      Funktor n ->
        pure expr `appE` (pure (fmapN n) `appE` (varE '(>>=) `sectionR` varE f) `appE` varE name)

    fmapN :: Int -> Exp
    fmapN n = foldr1 (\a b -> VarE '(.) `AppE` a `AppE` b) $ replicate n (VarE 'fmap)

  matches <- for bnds bind
  pure $ LamE [VarP x, VarP f] (CaseE (VarE x) matches)

stripLast2 :: Type -> Maybe Type
stripLast2 (a `AppT` b `AppT` _ `AppT` d)
  | AppT{} <- d = Nothing
  | otherwise   = Just (a `AppT` b)
stripLast2 _ = Nothing

-- Returns candidate
getPure :: Name -> [TyVarBndr] -> [Con] -> Q Name
getPure _name tyvr cons= do
  let
    findReturn :: Type -> [(Name, [Type])] -> Name
    findReturn ty constrs =
      case [ constr | (constr, [ty']) <- constrs, ty' == ty ] of
        []  -> error "Too few candidates for a variable constructor."
        [x] -> x
        --   data Exp a = Var1 a | Var2 a | ...
        -- result in
        --   Too many candidates: Var1, Var2
        xs  -> error ("Too many candidates: " ++ intercalate ", " (map pprint xs))

    -- Gets the last type variable, given 'data Exp a b c = ...'
    --
    --   lastTyVar = c
    lastTyVar :: Type
    lastTyVar = VarT (last (typeVars tyvr))

    allTypeArgs :: Con -> (Name, [Type])
    allTypeArgs con = case con of
      NormalC conName tys ->
        (conName, [ ty |    (_, ty) <- tys ])
      RecC conName tys ->
        (conName, [ ty | (_, _, ty) <- tys ])
      InfixC (_, t1) conName (_, t2) ->
        (conName, [ t1, t2 ])
      ForallC _ _ conName ->
         allTypeArgs conName
#if MIN_VERSION_template_haskell(2,11,0)
      _ -> error "Not implemented"
#endif

  return (findReturn lastTyVar (allTypeArgs `fmap` cons))
#else
#endif

-------------------------------------------------------------------------------
-- Type mangling
-------------------------------------------------------------------------------

-- | Extraty type variables
typeVars :: [TyVarBndr] -> [Name]
typeVars = map varBindName

varBindName :: TyVarBndr -> Name
varBindName (PlainTV n)    = n
varBindName (KindedTV n _) = n

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)
