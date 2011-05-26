{-# LANGUAGE GADTs, FlexibleInstances, ScopedTypeVariables, RankNTypes, StandaloneDeriving #-}
--
-- De Bruijn form of AST
--

module AST where
-- Standard libraries
import Text.Printf

data Exp a where
  Lvar  :: Int   -> Exp a -- a variable bound by a let node. Represented by a de Bruijn index.
  Var   :: Int   -> Exp a -- a variable bound by a lambda. Represented by de Bruijn index.
  Lam   :: Exp b -> Exp (a -> b)
  App   :: Exp (a -> b) -> Exp a -> Exp b
  Let   :: Exp bound  -- bound expression
        -> Exp body   -- the bound expressions scope.
        -> Exp body
  Const :: Show a => a -> Exp a
  Add   :: Exp a -> Exp a -> Exp a
  Cond  :: Exp Bool -> Exp a -> Exp a -> Exp a
  Eq    :: Exp a -> Exp a -> Exp Bool


deriving instance Show (Exp t)

--
-- Pretty printing
--
ppExp :: Exp a -> String
ppExp = go 0
  where
    go :: Int -> Exp a -> String
    go lvl (Lvar i) = printf "x%s" (lvl - i)
    go _ (Var i)         = printf "{%s}" (show i)
    go lvl (Lam body)   = printf "\\%s" (go (lvl+1) body)
    go lvl (App e1 e2)  = printf "(%s) (%s)" (go lvl e1) (go lvl e2)
    go lvl (Let bound body) = printf "let a%s = (%s) in (%s)"
                                (show lvl) (go lvl bound) (go (lvl+1) body)
    go _ (Const i) = show i
    go lvl (Add e1 e2) = printf ("%s + %s") (go lvl e1) (go lvl e2)
    go lvl (Cond c t e) = printf ("if %s then %s else %s") (go lvl c) (go lvl t) (go lvl e)
    go lvl (Eq e1 e2)   = printf "(%s) == (%s)" (go lvl e1) (go lvl e2)