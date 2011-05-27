{-# LANGUAGE GADTs, FlexibleInstances, ScopedTypeVariables, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

--
-- De Bruijn form of AST
--

module AST (
 Exp(..), ppExp, inlineExp
) where

-- Standard libraries
import Text.Printf
import Data.Typeable


--
-- Lambda calculus with let expressions, arithmetic and conditionals. Uses de Bruijn indices
-- for variables
--
data Exp a where
  Lvar  :: Int   -> Exp a -- a variable bound by a let node. Represented by a de Bruijn index.
  Var   :: Int   -> Exp a -- a variable bound by a lambda. Represented by de Bruijn index.
  Lam   :: Typeable b => Exp b -> Exp (a -> b)
  App   :: Typeable a => Exp (a -> b) -> Exp a -> Exp b
  -- Because type variable @bound@ does not appear in the output type @Exp body@ it is very
  -- easy to create @Let@ terms which are not type safe. You need to make sure that the
  -- corresponding @Lvar@ in the body expression has type @Exp bound@.
  Let   :: Typeable bound
        => Exp bound  -- bound expression
        -> Exp body   -- the bound expression's scope.
        -> Exp body
  Const :: Show a => a -> Exp a
  Add   :: Exp a -> Exp a -> Exp a
  Cond  :: Exp Bool -> Exp a -> Exp a -> Exp a
  Eq    :: Typeable a => Exp a -> Exp a -> Exp Bool

deriving instance Typeable1 Exp
deriving instance Show (Exp t)

--
-- Inlines the bound expression of @Let@ nodes into the bodies.
-- Useful for determining whether the sharing recovery algorithm is correct.
--
inlineExp :: Typeable a => Exp a -> Exp a
inlineExp = go []
  where
    go :: Typeable b => [WExp] -> Exp b -> Exp b
    go env v@(Lvar i) = case env !! i of
      WExp expr ->
        case gcast expr of
          Just exp -> exp
          Nothing  -> error ("body expression of let is not of appropriate type: " ++
                              show (typeOf expr) ++ " expecting: " ++ show (typeOf v))
    go _ v@(Var _) = v
    go env (App e1 e2) = App (go env e1) (go env e2)
    go env (Lam body) = Lam (go env body)
    go env (Let bound body) =
      let bound' = go env bound
      in  go (WExp bound':env) body
    go _ c@(Const _) = c
    go env (Add e1 e2) = Add (go env e1) (go env e2)
    go env (Cond cnd thn els) = Cond (go env cnd) (go env thn) (go env els)
    go env (Eq e1 e2) = Eq (go env e1) (go env e2)

data WExp where
  WExp :: Typeable a => Exp a -> WExp

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