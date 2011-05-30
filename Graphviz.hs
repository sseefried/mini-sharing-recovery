{-# LANGUAGE GADTs, PatternGuards, RankNTypes, ScopedTypeVariables, KindSignatures #-}
--
-- A module for pretty priting sharing recovering graphs
--
module Graphviz where

-- standard libraries
import System.Mem.StableName
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Typeable
import Unsafe.Coerce

-- friends
import HOAS
import Sharing hiding (StableExpName)

data GraphNodeType = SharingExpType | ExpType | FunType | SharingFunType | PreExpType

data GraphNodeDescriptor = GraphNodeDescriptor
                             GraphNodeType  -- type
                             Int            -- hash of stable name
                             Int            -- position in lookup list

data GraphStableName  where
  StableSharingExpName :: Typeable a => StableName (SharingExp a)     -> GraphStableName
  StableSharingFunName :: Typeable a => StableName (SharingFun a)     -> GraphStableName
  StableExpName        :: Typeable a => StableName (Exp a)            -> GraphStableName
  StableFunName        :: Typeable a => StableName (Fun a)            -> GraphStableName
  StablePreExpName     :: (Typeable a, Typeable1 exp, Typeable1 fun)
                       => StableName (PreExp exp fun a) -> GraphStableName

dotSharingExp :: SharingExp a -> IO [String]
dotSharingExp sharingExp = return []

type GraphNodes = IntMap [GraphStableName]
data Graph = Graph { nodes :: IntMap GraphNodeDescriptor }

-- Look up the occurence map keyed by array computations using a stable name.  If a the key does
-- not exist in the map, return an occurence count of '1'.
--
lookupGraphNode :: GraphStableName -> GraphNodes -> Maybe GraphNodeDescriptor
lookupGraphNode sa@(StableSharingExpName sn) = go SharingExpType sn sa
lookupGraphNode sa@(StableSharingFunName sn) = go SharingFunType sn sa
lookupGraphNode sa@(StableExpName sn)        = go ExpType        sn sa
lookupGraphNode sa@(StableFunName sn)        = go FunType        sn sa
lookupGraphNode sa@(StablePreExpName sn)     = go PreExpType     sn sa

go :: GraphNodeType -> StableName a -> GraphStableName -> GraphNodes -> Maybe GraphNodeDescriptor
go gnType sn sa nds = do
  let hash = hashStableName sn
  gsns <- IntMap.lookup hash nds
  i   <- findIndex (==sa) gsns
  return (GraphNodeDescriptor gnType hash i)

instance Eq GraphStableName where
  (StableSharingExpName sn1) == (StableSharingExpName sn2) = compareSN sn1 sn2
  (StableSharingFunName sn1) == (StableSharingFunName sn2) = compareSN sn1 sn2
  (StableExpName        sn1) == (StableExpName        sn2) = compareSN sn1 sn2
  (StableFunName        sn1) == (StableExpName        sn2) = compareSN sn1 sn2
  (StablePreExpName     sn1) == (StablePreExpName     sn2) = compareStableNamePreExp sn1 sn2
  _                          == _                          = False

compareSN :: (Typeable a, Typeable b) => StableName a -> StableName b -> Bool
compareSN sn1 sn2
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise                = False

compareStableNamePreExp :: (Typeable1 exp, Typeable1 fun, Typeable a,
                                            Typeable1 exp', Typeable1 fun', Typeable b)
          => StableName (PreExp exp fun a) -> StableName (PreExp exp' fun' b) -> Bool
compareStableNamePreExp pexp1 pexp2 = case gcasted of
  Just pexp1' -> pexp1' == pexp2
  Nothing     -> False
  where
    gcasted =
      if typeOf3' (getArg pexp1) == typeOf3' (getArg pexp2)
        then Just $ unsafeCoerce pexp1
        else Nothing
    typeOf3' :: forall exp fun a. (Typeable1 exp, Typeable1 fun, Typeable a)
             => PreExp (exp :: * -> *) (fun :: * -> *) a -> TypeRep
    typeOf3' _ = mkTyConApp (mkTyCon "HOAS.PreExp") []
                `mkAppTy` typeOf1 (undefined :: exp a)
                `mkAppTy` typeOf1 (undefined :: fun a) `mkAppTy` typeOf (undefined :: a)
    getArg :: c x -> x
    getArg = undefined