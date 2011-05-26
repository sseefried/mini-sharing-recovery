{-# LANGUAGE GADTs, FlexibleInstances, ScopedTypeVariables, RankNTypes, StandaloneDeriving #-}
--
-- De Bruijn form of AST
--

module AST (
 Exp(..), ppExp
) where
-- Standard libraries
import Text.Printf
import Data.Char

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
ppExp = go "" []
  where
    go :: String -> [String] -> Exp a -> String
    go _ lvl (Lvar i)         = printf "%s" (lvl !! i)
    go _ _ (Var i)            = printf "{%s}" (show i)
    go v lvl (Lam body)       = printf "\\%s" (go v lvl body)
    go v lvl (App e1 e2)      = printf "(%s) (%s)" (go v lvl e1) (go v lvl e2)
    go v lvl (Let bound body) =
      let boundPrefix = 'x':v
          bodyPrefix = 'y':v
      in printf "let %s = (%s) in (%s)" boundPrefix (go boundPrefix lvl bound)
                                             (go bodyPrefix (boundPrefix:lvl) body)
    go _ _ (Const i)          = show i
    go v lvl (Add e1 e2)      = printf ("%s + %s") (go v lvl e1) (go v lvl e2)

    go v lvl (Cond c t e)     = printf ("if %s then %s else %s")
                                  (go v lvl c) (go v lvl t) (go v lvl e)
    go v lvl (Eq e1 e2)       = printf "(%s) == (%s)" (go v lvl e1) (go v lvl e2)

----------

-- Tests
cnst :: Int -> Exp Int
cnst = Const

-- invalid term
test1, test2, test3 :: Exp Int
test1 = Let (Let (Lvar 0) (cnst 1)) (cnst 2)
test2 = Let (Let (cnst 1) (Lvar 0)) (Lvar 0)
test3 = Let (Let (cnst 1) (Lvar 0)) (Let (cnst 2) (Add (Lvar 0) (Lvar 1)))

