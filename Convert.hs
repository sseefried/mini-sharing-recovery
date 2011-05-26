{-# LANGUAGE GADTs, ScopedTypeVariables, PatternGuards #-}
module Convert where

-- standard libraries
import Data.Typeable
import Data.List
import System.Mem.StableName
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map

--friends
import Debug
import HOAS
import Sharing
import qualified AST as AST

--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Conversion of 'SharingExp's
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- 'TagEnv' maps the /unique identifier/, @n@, in expressions of form @Tag n@ and 'SharingFun's
-- of form @TaggedSharingExp n exp@ to an integer equal to the number of lambdas we were under
-- when we first saw @Tlam n exp@.
--
-- Unless the expression is malformed the 'TagEnv' should already contain an entry for @n@ when
-- @Tag n@ is encountered (since @Tag n@ is contained somewhere @exp@ in the function
-- @TaggedSharingExp n exp@ and the entry is inserted into the map when it is encountered).
--
type TagEnv = Map Int Int

convertSharingExp :: forall a. Typeable a
  => TagEnv             -- tag environment
  -> Int                -- current lambda environment size
  -> Int                -- current let environment size
  -> [StableSharingExp] -- let environment size
  -> SharingExp a
  -> AST.Exp a
convertSharingExp _ _ lenvSize lenv (VarSharing sa)
  | Just i <- findIndex (matchStableExp sa) lenv = AST.Lvar (lenvSize - i - 1)
  | otherwise = error ("convertSharingExp " ++ err)
  where
    err = printf "inconsistent valuation; sa = %s; env = %s"
            (show $ hashStableName sa) (show lenv)
convertSharingExp m envSize lenvSize lenv (LetSharing sa@(StableSharingExp _ boundExp) bodyExp) =
  AST.Let (convertSharingExp m envSize lenvSize lenv boundExp)
          (convertSharingExp m envSize (lenvSize+1) (sa:lenv) bodyExp)
--
-- This case of 'convertSharingExp' does the HOAS -> de Bruijn conversion.
--
convertSharingExp m envSize lenvSize lenv (ExpSharing _ preExpr) = cvtPreExp preExpr
   where
     cvtPreExp :: forall b.Typeable b => PreExp SharingExp SharingFun b -> AST.Exp b
     cvtPreExp preExp = case preExp of
       Tag i            -> case Map.lookup i m of
          Nothing -> error (printf "Value for unique tag '%d' not found. Should never happen." i)
          Just n  -> AST.Var (envSize - n - 1)
       App fun arg      -> AST.App   (convertSharingFun m envSize lenvSize lenv fun) (cvtExp arg)
       Const i          -> AST.Const i
       Add e1 e2        -> AST.Add   (cvtExp e1) (cvtExp e2)
       Cond cnd thn els -> AST.Cond  (cvtExp cnd) (cvtExp thn) (cvtExp els)
       Eq e1 e2         -> AST.Eq    (cvtExp e1) (cvtExp e2)
      where
        cvtExp :: Typeable c => SharingExp c -> AST.Exp c
        cvtExp = convertSharingExp m envSize lenvSize lenv

convertSharingFun :: forall a b. (Typeable a, Typeable b)
  => Map Int Int     -- TagEnv
  -> Int             -- lambda environment size
  -> Int             -- let environment size
  -> [StableSharingExp] -- let environment
  -> SharingFun (a -> b)
  -> AST.Exp (a -> b)
convertSharingFun m envSize lenvSize lenv (TaggedSharingExp n exp) =
  AST.Lam $ convertSharingExp (Map.insert n envSize m) (envSize+1) lenvSize lenv exp

--
-- ~~~~~~~~~~~~~~~~~~~~
-- Conversion of 'Exp's
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Then in @Tag n@ means something different in 'convertExp' to what they mean in
-- 'convertSharingExp'. Without out sharing recovery, just before conversion the expression
-- does not yet contain any @Tag@ constructors. They are only inserted into the AST when a
-- @Lam@ constructor is encountered in 'convertFun'. The value assigned to them is the number
-- of lambdas we are under at that point (equal to @envSize@).
--
convertExp :: forall a. Typeable a => Int -> Exp a -> AST.Exp a
convertExp envSize (Exp preExpr) = cvtPreExp preExpr
   where
     cvtPreExp :: forall b.Typeable b => PreExp Exp Fun b -> AST.Exp b
     cvtPreExp preExp = case preExp of
       Tag i            -> AST.Var   (envSize - i - 1)
       App fun arg      -> AST.App   (convertFun envSize fun) (cvtExp arg)
       Const i          -> AST.Const i
       Add e1 e2        -> AST.Add   (cvtExp e1) (cvtExp e2)
       Cond cnd thn els -> AST.Cond  (cvtExp cnd) (cvtExp thn) (cvtExp els)
       Eq e1 e2         -> AST.Eq    (cvtExp e1) (cvtExp e2)
      where
        cvtExp :: Typeable c => Exp c -> AST.Exp c
        cvtExp = convertExp envSize

convertFun :: forall a. Typeable a => Int -> Fun a -> AST.Exp a
convertFun envSize (Lam fun) = AST.Lam $ convertExp (envSize+1) (fun (Exp $ Tag envSize))

-----------

sharingConvert :: Typeable a => Exp a -> AST.Exp a
sharingConvert expr = convertSharingExp Map.empty 0 0 [] $ recoverSharing expr

convert :: Typeable a => Exp a -> AST.Exp a
convert expr = convertExp 0 expr

eqTest :: (Typeable a) => Exp a -> IO Bool
eqTest expr = do
  let a = sharingConvert expr
      b = convert expr
      equal = show a == show b
  if equal
    then return True
    else do
      printf "Not equal:\n%s\n/=\n%s" (show a) (show b)
      return False

-------

t1 :: Exp Int
t1 = app (lam (\x -> app (lam $ \y -> x + y) (constant (1::Int)))) (constant (2::Int))

t2 :: Exp Int
t2 = let c = constant (2::Int)
         a = app (lam (\x -> c + c + x)) c
     in app (lam (\y -> app (lam (\x -> x + y + 1)) a)) a

t3 :: Exp Int
t3 = let c = constant (1::Int)
         a = app (lam (\x -> c + c + x)) c
     in (app (lam (\x -> x + 1)) a) + a

--
-- You should be able to recover sharing under lambdas. Here is an example
--
t4 :: Exp Int
t4 = app (lam (\x -> a + a + x)) 728
  where a = 42 + 666