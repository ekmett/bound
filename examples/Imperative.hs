{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Imperative where

-- It's possible to use bound "sideways" in order to support terms which do not
-- have a Monad instance. A typical situation in which this would happen is when
-- modelling an imperative language: variables are bound by statements, but they
-- are used in positions where it would make no sense to replace them by another
-- statement.

import Bound
import Control.Monad (ap)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity
import Data.IORef
import Data.Void (Void, absurd)


-- PART 1: We want to model a tiny assembly language.
-- 
--   %0 = add 1 2
--   %1 = add %0 %0
--   ret %1
-- 
-- Add binds a fresh variable, and its operands can either be literals or
-- previously-bound variables. Ret must be the last instruction.
-- 
-- Operand is monadic, traversable, and satisfies all the other requirements in
-- order to be used with bound. But this is not sufficient, since Operand is
-- not the whole language: we also need to define Prog, the sequence of
-- instructions.
data Operand a
  = Lit Int
  | Var a
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Applicative Operand where
  pure = Var
  (<*>) = ap

instance Monad Operand where
  Lit i >>= _ = Lit i
  Var x >>= f = f x

-- The following definition correctly models the instructions and their free
-- variables. But since the Var in Operand cannot be replaced with a Prog, this
-- definition is not monadic, and so we cannot manipulate the (Scope () Prog a)
-- using bound's functions. This defeats the point of using Scope at all!
-- 
--   data Prog a
--     = Ret (Operand a)
--     | Add (Operand a) (Operand a)
--           (Scope () Prog a) -- one more bound variable, available
--                             -- in the rest of the program
-- 
-- The sideways trick is to replace the Operand constructor with a (* -> *) type
-- parameter. Instantiating this with the real Operand will allow Operand to
-- access the same free variables as Prog. But if we instantiating this with
-- (Scope () Operand) instead, then the operands will have access to one extra
-- bound variable! This way, we can bind fresh variables which can only be used
-- inside the operands, and not in Prog.
data Prog operand a
  = Ret (operand a)
  | Add (operand a) (operand a)
        (Prog (Scope () operand) a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

-- The fact that the variables are not available in Prog after they are bound
-- might seem strange, and we'll fix this in part 2, but it is actually a good
-- thing. We want to be able to replace those variables with operand values, and
-- that would not be possible if variables were allowed to appear inside Prog
-- but outside of an operand.
pInstantiate1 :: Monad operand
              => operand a
              -> Prog (Scope () operand) a
              -> Prog operand a
pInstantiate1 x (Ret o)        = Ret (instantiate1 x o)
pInstantiate1 x (Add o1 o2 cc) = Add (instantiate1 x o1)
                                     (instantiate1 x o2)
                                     (pInstantiate1 (lift x) cc)

pAbstract1 :: (Monad operand, Eq a)
           => a
           -> Prog operand a
           -> Prog (Scope () operand) a
pAbstract1 x (Ret o)        = Ret (abstract1 x o)
pAbstract1 x (Add o1 o2 cc) = Add (abstract1 x o1)
                                  (abstract1 x o2)
                                  (pAbstract1 x cc)

evalOperand :: Operand Void -> Int
evalOperand (Lit i)    = i
evalOperand (Var void) = absurd void

-- |
-- >>> :{
-- let Just prog = closed
--               $ Add (Lit 1) (Lit 2)       $ pAbstract1 "%0"
--               $ Add (Var "%0") (Var "%0") $ pAbstract1 "%1"
--               $ Ret (Var "%1")
-- :}
-- 
-- >>> evalProg prog
-- 6
evalProg :: Prog Operand Void -> Int
evalProg (Ret o)        = evalOperand o
evalProg (Add o1 o2 cc) = evalProg cc'
  where
    result :: Int
    result = evalOperand o1 + evalOperand o2
    
    cc' :: Prog Operand Void
    cc' = pInstantiate1 (Lit result) cc


-- PART 2: Here's a slightly more complicated language.
-- 
--   %0 = add 1 2
--   %1 = add %0 %0
--   swp %0 %1
--   ret %1
-- 
-- The new swp command swaps the contents of two variables, so the two arguments
-- must be previously-bound variables, they cannot be literals. This time the
-- naïve definition looks like this:
-- 
--   data Prog' a
--     = Ret' (Operand a)
--     | Swp' a a
--            (Prog' a)
--     | Add' (Operand a) (Operand a)
--            (Scope () Prog' a)
-- 
-- If we apply the sideways trick to this definition, the newly-bound variables
-- will only be available in the operands, and so it won't be possible to call
-- swp on them. The first step towards a solution is to add seemingly-useless
-- Identity wrappers:
-- 
--   data Prog' a
--     = Ret' (Operand a)
--     | Swp' (Identity a) (Identity a)
--            (Prog' a)
--     | Add' (Operand a) (Operand a)
--            (Scope () Prog' a)
-- 
-- We can now apply the sideways trick twice: once for Operand, and once for
-- Identity. This gives us a lot of control: we can bind fresh variables which
-- can only be used inside the operands, we can bind fresh variables which can
-- be used inside Prog but not inside the operands, and as required for this
-- example, we can bind fresh variables which can be used in both.
data Prog' operand identity a
  = Ret' (operand a)
  | Swp' (identity a) (identity a)
         (Prog' operand identity a)
  | Add' (operand a) (operand a)
         (Prog' (Scope () operand) (Scope () identity) a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

-- Bound variables can now occur in both operand and identity, so we can no
-- longer instantiate them with operands. Instead, we'll have to instantiate
-- them with a value which both (Operand a) and (Identity a) can contain:
-- a free variable.
pInstantiate1' :: (Monad operand, Monad identity)
               => a
               -> Prog' (Scope () operand) (Scope () identity) a
               -> Prog' operand identity a
pInstantiate1' x (Ret' o)        = Ret' (instantiate1 (pure x) o)
pInstantiate1' x (Swp' i1 i2 cc) = Swp' (instantiate1 (pure x) i1)
                                        (instantiate1 (pure x) i2)
                                        (pInstantiate1' x cc)
pInstantiate1' x (Add' o1 o2 cc) = Add' (instantiate1 (pure x) o1)
                                        (instantiate1 (pure x) o2)
                                        (pInstantiate1' x cc)

pAbstract1' :: (Monad operand, Monad identity, Eq a)
            => a
            -> Prog' operand identity a
            -> Prog' (Scope () operand) (Scope () identity) a
pAbstract1' x (Ret' o)        = Ret' (abstract1 x o)
pAbstract1' x (Swp' o1 o2 cc) = Swp' (abstract1 x o1)
                                     (abstract1 x o2)
                                     (pAbstract1' x cc)
pAbstract1' x (Add' o1 o2 cc) = Add' (abstract1 x o1)
                                     (abstract1 x o2)
                                     (pAbstract1' x cc)

evalOperand' :: Operand (IORef Int) -> IO Int
evalOperand' (Lit i)   = return i
evalOperand' (Var ref) = readIORef ref

-- |
-- >>> :{
-- let Just prog' = closed
--                $ Add' (Lit 1) (Lit 2)       $ pAbstract1' "%0"
--                $ Add' (Var "%0") (Var "%0") $ pAbstract1' "%1"
--                $ Swp' (Identity "%0") (Identity "%1")
--                $ Ret' (Var "%1")
-- :}
-- 
-- >>> evalProg' prog'
-- 3
evalProg' :: Prog' Operand Identity (IORef Int) -> IO Int
evalProg' (Ret' o)        = evalOperand' o
evalProg' (Swp' (Identity ref1) (Identity ref2) cc) = do
    x <- readIORef ref1
    y <- readIORef ref2
    writeIORef ref1 y
    writeIORef ref2 x
    evalProg' cc
evalProg' (Add' o1 o2 cc) = do
    result <- (+) <$> evalOperand' o1 <*> evalOperand' o2
    ref <- newIORef result
    evalProg' (pInstantiate1' ref cc)


main :: IO ()
main = return ()
