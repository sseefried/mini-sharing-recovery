{-# LANGUAGE GADTs, ScopedTypeVariables, PatternGuards #-}
module Convert where

-- standard libraries
import Data.Typeable
import Data.List
import System.Mem.StableName
import Text.Printf

--friends
import Debug
import HOAS
import Sharing
import qualified AST as AST

convertSharingExp :: forall a. Typeable a =>
     Int                -- current lambda environment size
  -> Int                -- current let environment size
  -> [StableSharingExp] -- let environment size
  -> SharingExp a
  -> AST.Exp a
convertSharingExp _ lenvSize lenv (VarSharing sa)
  | Just i <- findIndex (matchStableExp sa) lenv = AST.Lvar (lenvSize - i - 1)
  | otherwise = error ("convertSharingExp " ++ err)
  where
    err = printf "inconsistent valuation; sa = %s; env = %s"
            (show $ hashStableName sa) (show lenv)
convertSharingExp envSize lenvSize lenv (LetSharing sa@(StableSharingExp _ boundExp) bodyExp) =
  AST.Let (convertSharingExp envSize lenvSize lenv boundExp)
          (convertSharingExp envSize (lenvSize+1) (sa:lenv) bodyExp)
--
-- This case of 'convertSharingExp' does the HOAS -> de Bruijn conversion.
--
convertSharingExp envSize lenvSize lenv (ExpSharing _ preExpr) = cvtPreExp preExpr
   where
     cvtPreExp :: forall b.Typeable b => PreExp SharingExp b -> AST.Exp b
     cvtPreExp preExp = case preExp of
       Tag i            -> AST.Var   (envSize - i - 1)
       Ltag i           -> AST.Lvar  (lenvSize - i - 1)
       App fun arg      -> AST.App   (convertSharingFun envSize lenvSize lenv fun) (cvtExp arg)
       Const i          -> AST.Const i
       Add e1 e2        -> AST.Add   (cvtExp e1) (cvtExp e2)
       Cond cnd thn els -> AST.Cond  (cvtExp cnd) (cvtExp thn) (cvtExp els)
       Eq e1 e2         -> AST.Eq    (cvtExp e1) (cvtExp e2)
      where
        cvtExp :: Typeable c => SharingExp c -> AST.Exp c
        cvtExp = convertSharingExp envSize lenvSize lenv

convertSharingFun :: forall a b. (Typeable a, Typeable b)
  => Int 
  -> Int
  -> [StableSharingExp]
  -> PreFun SharingExp (a -> b)
  -> AST.Exp (a -> b)
convertSharingFun envSize lenvSize lenv (Lam fun) = AST.Lam $ convertSharingExp (envSize+1)
                                                                lenvSize lenv (fun undefined)

-----

convertExp :: forall a. Typeable a => Int -> Exp a -> AST.Exp a
convertExp envSize (Exp preExpr) = cvtPreExp preExpr
   where
     cvtPreExp :: forall b.Typeable b => PreExp Exp b -> AST.Exp b
     cvtPreExp preExp = case preExp of
       Tag i            -> AST.Var   (envSize - i - 1)
       Ltag _           -> error "should not appear"
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
sharingConvert expr = convertSharingExp 0 0 [] $ recoverSharing expr

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

t :: Exp Int
t = app (lam (\x -> app (lam (\y -> x + y)) (constant (1::Int)))) (constant (2::Int))

--
-- This causes a problem. We cannot know, during sharing recovery what Tag the \x in 
--   a = app (lam (\x -> c + c + x)) gets. This can only be worked out when we know 
--   where it is being used.
-- 

t2 :: Exp Int
t2 = let c = constant (2::Int)
         a = app (lam (\x -> c + c + x)) c
     in app (lam (\_ -> app (lam (\x -> x + 1)) a)) a

t3 :: Exp Int
t3 = let c = constant (1::Int)
         a = app (lam (\x -> c + c + x)) c
     in (app (lam (\x -> x + 1)) a) + a
     