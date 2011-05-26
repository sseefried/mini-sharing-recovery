--
-- Author: Sean Seefried
--
-- This module contains a minimal language.
--
{-# LANGUAGE GADTs, StandaloneDeriving, RankNTypes, FlexibleInstances, DeriveDataTypeable #-}
module HOAS where

-- standard libraries
import Data.Typeable
import Text.Printf

-- friends

class (Eq a, Show a, Typeable a) => Elt a

instance Elt Int
instance Elt Float
instance Elt Bool

--
-- We "pull out" lambda terms from the 'PreExp' data type. We want
-- all expressions to evaluate to a primitive value satisfying type class Elt a.
--
data Fun t where
  Lam   :: (Elt a, Elt b) => (Exp a -> Exp b) -> Fun (a -> b)

data PreExp exp fun t where
  Tag   :: Elt a => Int -> PreExp exp fun a -- ^ tag for lambda bound variables.
                                            -- Only used during conversion and sharing recovery.
  App   :: (Elt a, Elt b) => fun (a -> b) -> exp a -> PreExp exp fun b
  Const :: Elt a => a -> PreExp exp fun a
  Add   :: Elt a => exp a -> exp a -> PreExp exp fun a
  Cond  :: Elt a => exp Bool -> exp a -> exp a -> PreExp exp fun a
  Eq    :: Elt a => exp a -> exp a -> PreExp exp fun Bool

deriving instance Typeable1 Exp

newtype Exp a = Exp (PreExp Exp Fun a) deriving Eq

instance Show (Exp a) where
  show (Exp pexp) = show pexp

instance Show (PreExp Exp Fun a) where
  show (Tag  i)     = printf "Tag %s" (show i)
  show (App _ e2)   = printf "App <fun> (%s)" (show e2)
  show (Const i)    = printf "Const %s" (show i)
  show (Add e1 e2)  = printf "Add (%s) (%s)" (show e1) (show e2)
  show (Cond c t e) = printf "Cond (%s) (%s) (%s)" (show c) (show t) (show e)
  show (Eq e1 e2)   = printf "Eq (%s) (%s)" (show e1) (show e2)

instance Elt a => Eq (PreExp exp fun a) where
  (==) = error "not defined"

instance (Num a, Elt a) => Num (Exp a) where
  a + b = Exp $ Add a b
  fromInteger a = constant (fromInteger a)
  (*) = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"
--
-- Smart constructors
--
lam :: (Elt a, Elt b) => (Exp a -> Exp b) -> Fun (a -> b)
lam fun = Lam fun

app :: (Elt a, Elt b) => Fun (a -> b) -> Exp a -> Exp b
app e1 e2 = Exp $ App e1 e2

constant :: Elt a => a -> Exp a
constant = Exp . Const

(?) :: Elt a => Exp Bool -> (Exp a, Exp a) -> Exp a
c ? (t,e) = Exp $ Cond c t e

(==*) :: Elt a => Exp a -> Exp a -> Exp Bool
a ==* b = Exp $ Eq a b

showOp :: PreExp exp fun a -> String
showOp (Tag _)   = "Tag"
showOp (App _ _) = "App"
showOp (Const _) = "Const"
showOp (Add _ _) = "Add"
showOp (Cond _ _ _) = "Cond"
showOp (Eq _ _) = "Eq"


--
-- Pretty printing
--

ppExp :: Exp a -> String
ppExp = go 0
  where
    go :: Int -> Exp a -> String
    go lvl (Exp pexp)  = ppPreExp (flip go) lvl pexp

ppPreExp :: (forall b.exp b -> Int -> String)
        -> Int -- let level
        -> (PreExp exp fun a)
        -> String
ppPreExp _ _ (Tag _) = error "only used during conversion"
ppPreExp pp lvl (App _ e2)  = printf "<fun> (%s)" (pp e2 lvl)
ppPreExp _ _ (Const i) = show i
ppPreExp pp lvl (Add e1 e2) = printf ("%s + %s") (pp e1 lvl) (pp e2 lvl)
ppPreExp pp lvl (Cond c t e) = printf ("if (%s) then (%s) else (%s)")
                                 (pp c lvl) (pp t lvl) (pp e lvl)
ppPreExp pp lvl (Eq e1 e2) = printf ("%s == %s") (pp e1 lvl) (pp e2 lvl)
