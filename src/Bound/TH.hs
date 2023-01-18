{-# LANGUAGE CPP           #-}
{-# LANGUAGE PatternGuards #-}

#if __GLASGOW_HASKELL__ >= 900
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

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
import Language.Haskell.TH.Datatype.TyVarBndr

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))

-- |
-- Use to automatically derive 'Applicative' and 'Monad' instances for
-- your datatype.
--
-- Also works for components that are lists or instances of 'Functor',
-- but still does not work for a great deal of other things.
--
-- The @deriving-compat@ package may be used to derive the 'Show1' and 'Read1'
-- instances. Note that due to Template Haskell staging restrictions, we must
-- define these instances within the same TH splice as the 'Show' and 'Read'
-- instances. (This is needed for GHC 9.6 and later, where 'Show' and 'Read'
-- are quantified superclasses of 'Show1' and 'Read1', respectively.)
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
--
-- concat <$> sequence
--   [ deriveShow1 ''Exp
--   , deriveRead1 ''Exp
--   , [d| instance Read a => Read (Exp a) where readsPrec = readsPrec1
--         instance Show a => Show (Exp a) where showsPrec = showsPrec1
--       |]
--   ]
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
-- ghci| fmap concat $ sequence [deriveShow1 ''Exp, deriveRead1 ''Exp, [d| instance Read a => Read (Exp a) where { readsPrec = readsPrec1 }; instance Show a => Show (Exp a) where { showsPrec = showsPrec1 } |]]
-- ghci| :}
-- @
--
-- The 'Eq' and 'Ord' instances can be derived similarly:
--
-- @
-- import Data.Functor.Classes (Eq1, Ord1, eq1, compare1)
-- import Data.Deriving        (deriveEq1, deriveOrd1)
--
-- fmap concat $ sequence
--   [ deriveEq1 ''Exp
--   , deriveOrd1 ''Exp
--   , [d| instance Eq a => Eq (Exp a) where (==) = eq1
--         instance Ord a => Ord (Exp a) where compare = compare1
--       |]
--   ]
-- @
--
-- or in GHCi:
--
-- @
-- ghci> import Data.Functor.Classes (Eq1, Ord1, eq1, compare1)
-- ghci> import Data.Deriving        (deriveEq1, deriveOrd1)
-- ghci> :{
-- ghci| fmap concat $ sequence [deriveEq1 ''Exp, deriveOrd1 ''Exp, [d| instance Eq a => Eq (Exp a) where { (==) = eq1 }; instance Ord a => Ord (Exp a) where { compare = compare1 } |]]
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
    DataD _ _name vars _ cons _ -> makeBound' name vars cons
    _ -> fail $ show name ++ " Must be a data type."

makeBound' :: Name -> [TyVarBndrUnit] -> [Con] -> DecsQ
makeBound' name vars cons = do
  let instanceHead :: Type
      instanceHead = name `conAppsT` map VarT (typeVars (init vars))

      var  :: ExpQ
      var  = ConE `fmap` getPure name vars cons

      bind :: ExpQ
      bind = constructBind name vars cons

  [d| instance Applicative $(pure instanceHead) where
        pure = $var
        {-# INLINE pure #-}

        ff <*> fy = do
          f <- ff
          y <- fy
          pure (f y)
        {-# INLINE (<*>) #-}

      instance Monad $(pure instanceHead) where
        (>>=)  = $bind
        {-# INLINE (>>=) #-}
    |]

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

constructBind :: Name -> [TyVarBndrUnit] -> [Con] -> ExpQ
constructBind name vars cons = do
  interpret =<< construct name vars cons

construct :: Name -> [TyVarBndrUnit] -> [Con] -> Q [Components]
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
  isKonst (VarT n) = n /= tvName (last vars)
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
       (ConP name
#if MIN_VERSION_template_haskell(2,18,0)
             []
#endif
             [ VarP arg | (arg, _) <- bounds ])
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
getPure :: Name -> [TyVarBndrUnit] -> [Con] -> Q Name
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
      _ -> error "Not implemented"

  return (findReturn lastTyVar (allTypeArgs `fmap` cons))

-------------------------------------------------------------------------------
-- Type mangling
-------------------------------------------------------------------------------

-- | Extract type variables
typeVars :: [TyVarBndr_ flag] -> [Name]
typeVars = map tvName

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)
#else
#endif
