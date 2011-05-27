{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Main where

-- standard libraries
import Prelude hiding (exp)
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion)
import qualified Test.HUnit as HU
import Test.QuickCheck

-- friends
import qualified AST as AST
import qualified HOAS as HOAS
import Convert

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [ testGroup "AST tests" 
            [ testCase "show 1" testAstShow1
            , testCase "show 2" testAstShow2
            , testCase "inline 1" testAstInline1
            , testCase "inline 2" testAstInline2 
            ]
         , testGroup "HOAS tests" 
             [ testCase "show 1" testHoasShow1A
             , testCase "show 1" testHoasShow1B
             ]
         , testGroup "Sharing recovery" 
             ([ testProperty "works" propInlineSharingRecoveryEquality ] ++
             map (testCase "works" . testSharingRecovery) 
                       [ testHoas1, testHoas2, testHoas3, testHoas4, testHoas5, testHoas6 ])
         ]



cnst :: Int -> AST.Exp Int
cnst = AST.Const

testAst1, testAst2 :: AST.Exp Int
-- invalid term
testAst1 = AST.Let (AST.Let (cnst 1) (AST.Lvar 0) :: AST.Exp Int) (AST.Lvar 0)
testAst2 = AST.Let (AST.Let (cnst 1) (AST.Lvar 0) :: AST.Exp Int) 
                   (AST.Let (cnst 2) (AST.Add (AST.Lvar 0) (AST.Lvar 1)))

testAstShow1 :: Assertion
testAstShow1 =  (show testAst1) HU.@?= "Let (Let (Const 1) (Lvar 0)) (Lvar 0)" 

testAstShow2 :: Assertion
testAstShow2 = (show testAst2) HU.@?= 
  "Let (Let (Const 1) (Lvar 0)) (Let (Const 2) (Add (Lvar 0) (Lvar 1)))"

testAstInline1 :: Assertion
testAstInline1 = (show $ AST.inlineExp testAst1) HU.@?= "Const 1"

testAstInline2 :: Assertion
testAstInline2 = (show $ AST.inlineExp testAst2) HU.@?= "Add (Const 2) (Const 1)"

testHoas1 :: HOAS.Exp Int
testHoas1 = HOAS.app (HOAS.lam (\x -> x + 1)) 2

testHoas2 :: HOAS.Exp Int
testHoas2 = HOAS.app (HOAS.lam (\x -> HOAS.app (HOAS.lam $ \y -> x + y)  1)) 2


testHoas3 :: HOAS.Exp Int
testHoas3 = let c = 2
                a = HOAS.app (HOAS.lam (\x -> c + c + x)) c
            in HOAS.app (HOAS.lam (\y -> HOAS.app (HOAS.lam (\x -> x + y + 1)) a)) a

testHoas4 :: HOAS.Exp Int
testHoas4 = let c = 1
                a = HOAS.app (HOAS.lam (\x -> c + c + x)) c
     in (HOAS.app (HOAS.lam (\x -> x + 1)) a) + a

-- --
-- -- You should be able to recover sharing under lambdas. Here is an example
-- --
testHoas5 :: HOAS.Exp Int
testHoas5 = HOAS.app (HOAS.lam (\x -> a + a + x)) 728
    where a = 42 + 666

testHoas6 :: HOAS.Exp Int
testHoas6 = let c = (2 :: HOAS.Exp Int)
                d = c + c
            in HOAS.app (HOAS.lam (\x -> x + x + d)) d


testHoasShow1A :: Assertion
testHoasShow1A = testHoasShow testHoas1 "App (Lam (Add (Var 0) (Const 1))) (Const 2)"

testHoasShow1B :: Assertion 
testHoasShow1B = testHoasSharingShow testHoas1 "App (Lam (Add (Var 0) (Const 1))) (Const 2)"



--
-- QuickCheck instances
--

instance Arbitrary (HOAS.PreExp HOAS.Exp HOAS.Fun Bool) where
  arbitrary = oneof [ return (HOAS.Const True), return (HOAS.Const False) 
                    , return HOAS.Eq `ap` (arbitrary :: Gen (HOAS.Exp Int)) `ap` arbitrary ]

instance Arbitrary (HOAS.PreExp HOAS.Exp HOAS.Fun Int) where
  arbitrary = do 
    frequency [ (50, return HOAS.Const `ap` arbitrary)
              , (10, return HOAS.Add `ap` arbitrary `ap` (arbitrary :: Gen (HOAS.Exp Int)))
              -- introduces sharing
              , (10, return HOAS.App `ap` sharingLam `ap` arbitrary)
              , (10, return HOAS.Cond `ap` arbitrary `ap` arbitrary `ap` arbitrary)

              ]
   where
      sharingLam :: Gen (HOAS.Fun (Int -> Int))
      sharingLam =
         return $ HOAS.lam (\x -> x + x :: HOAS.Exp Int)


instance Arbitrary (HOAS.PreExp HOAS.Exp HOAS.Fun a) => Arbitrary (HOAS.Exp a) where
  arbitrary = return HOAS.Exp `ap` arbitrary

--
-- Helpers
--
testHoasShow :: HOAS.Exp Int -> String -> Assertion
testHoasShow exp expected = (show $ convert exp) HU.@?= expected

testHoasSharingShow :: HOAS.Exp Int -> String -> Assertion
testHoasSharingShow exp expected = (show $ sharingConvert exp) HU.@?= expected

--
-- Checks that straight conversion is the same as sharing recovery, followed by conversion, followed
-- by inlining of all lets.
--
propInlineSharingRecoveryEquality :: HOAS.Exp Int -> Bool
propInlineSharingRecoveryEquality exp = 
    (show $ convert exp) == (show $ AST.inlineExp $ sharingConvert exp)


testSharingRecovery :: HOAS.Exp Int -> Assertion
testSharingRecovery exp = (show $ convert exp) HU.@?= (show $ AST.inlineExp $ sharingConvert exp)