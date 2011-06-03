{-# LANGUAGE GADTs, PatternGuards, RankNTypes, ScopedTypeVariables, KindSignatures #-}
module Graph (
  GraphStableName(..),
  Graph, -- opaque
  -- * Functions
  newGraph, insertEdge, graphNodes, graphEdges

  -- instances 
  -- Eq (GraphStableName)

) where

-- standard libraries
import System.Mem.StableName
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Typeable
import Unsafe.Coerce
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf

-- friends
import HOAS
import Sharing hiding (StableExpName)

data GraphNodeType = SharingExpType | ExpType | FunType | SharingFunType | PreExpType
   deriving (Eq, Show, Ord)

-- By pairing the hash of the stable name and the position in the lookup list we
-- it allows us to have an Ord instance.
data GraphNode = GraphNode
                   GraphNodeType  -- type
                   String         -- name
                   Int            -- hash of stable name
                   Int            -- position in lookup list
   deriving (Eq, Show, Ord)

data GraphStableName  where
  StableSharingExpName :: Typeable a => String -> StableName (SharingExp a)     -> GraphStableName
  StableSharingFunName :: Typeable a => String -> StableName (SharingFun a)     -> GraphStableName
  StableExpName        :: Typeable a => String -> StableName (Exp a)            -> GraphStableName
  StableFunName        :: Typeable a => String -> StableName (Fun a)            -> GraphStableName
  StablePreExpName     :: (Typeable a, Typeable1 exp, Typeable1 fun)
                       => String -> StableName (PreExp exp fun a) -> GraphStableName


instance Show GraphStableName where
  show (StableSharingExpName nm sn) =
    printf "StableSharingExpName: %s %s" nm (show $ hashStableName sn)
  show (StableSharingFunName nm sn) =
    printf "StableSharingFunName: %s %s" nm (show $ hashStableName sn)
  show (StableExpName nm sn) =
    printf "StableExpName: %s %s" nm (show $ hashStableName sn)
  show (StableFunName nm sn) =
    printf "StableFunName: %s %s" nm (show $ hashStableName sn)
  show (StablePreExpName nm sn) =
    printf "StablePreExpName: %s %s" nm (show $ hashStableName sn)
--
-- Graphs
--

type GraphLookup = IntMap [GraphStableName]
data Graph = Graph { graphLookup :: GraphLookup
                   , graphNodes :: Set GraphNode
                   , graphEdges :: Map GraphNode (Set GraphNode) } deriving Show

newGraph :: Graph
newGraph = Graph { graphLookup = IntMap.empty, graphNodes = Set.empty, graphEdges = Map.empty }

insertEdge :: Graph -> (GraphStableName, GraphStableName) -> Graph
insertEdge g (src, tgt) = g { graphNodes = graphNodes', graphEdges = graphEdges' }
  where
    graphLookup' = foldl insertGraphLookup (graphLookup g) [src, tgt]
    srcNode = fromJust $ lookupGraphNode src graphLookup'
    tgtNode = fromJust $ lookupGraphNode tgt graphLookup'
    graphNodes' = foldr Set.insert (graphNodes g) [srcNode, tgtNode]
    graphEdges' =
       let tgtNodes = fromMaybe (Set.empty) $  Map.lookup srcNode (graphEdges g)
       in  Map.insert srcNode (Set.insert tgtNode tgtNodes) (graphEdges g)

insertGraphLookup :: GraphLookup -> GraphStableName -> GraphLookup
insertGraphLookup tbl gsn = IntMap.insert hash (gsns ++ [gsn]) tbl
  where
    hash = hashGraphStableName gsn
    gsns = fromMaybe [] $ IntMap.lookup hash tbl

hashGraphStableName :: GraphStableName -> Int
hashGraphStableName gsn = case gsn of
  StableSharingExpName _ sn -> hashStableName sn
  StableSharingFunName _ sn -> hashStableName sn
  StableExpName        _ sn -> hashStableName sn
  StableFunName        _ sn -> hashStableName sn
  StablePreExpName     _ sn -> hashStableName sn

--
-- Look up the occurence map keyed by array computations using a stable name.  If a the key does
-- not exist in the map, return an occurence count of '1'.
--
lookupGraphNode :: GraphStableName -> GraphLookup -> Maybe GraphNode
lookupGraphNode sa@(StableSharingExpName nm sn) = lookupGraphNodeAux SharingExpType nm sn sa
lookupGraphNode sa@(StableSharingFunName nm sn) = lookupGraphNodeAux SharingFunType nm sn sa
lookupGraphNode sa@(StableExpName nm sn)        = lookupGraphNodeAux ExpType        nm sn sa
lookupGraphNode sa@(StableFunName nm sn)        = lookupGraphNodeAux FunType        nm sn sa
lookupGraphNode sa@(StablePreExpName nm sn)     = lookupGraphNodeAux PreExpType     nm sn sa

lookupGraphNodeAux :: GraphNodeType -> String -> StableName a -> GraphStableName
                   -> GraphLookup -> Maybe GraphNode
lookupGraphNodeAux gnType nm sn sa nds = do
  let hash = hashStableName sn
  gsns <- IntMap.lookup hash nds
  i   <- findIndex (==sa) gsns
  return (GraphNode gnType nm hash i)

instance Eq GraphStableName where
  (StableSharingExpName _ sn1) == (StableSharingExpName _ sn2) = compareSN sn1 sn2
  (StableSharingFunName _ sn1) == (StableSharingFunName _ sn2) = compareSN sn1 sn2
  (StableExpName        _ sn1) == (StableExpName        _ sn2) = compareSN sn1 sn2
  (StableFunName        _ sn1) == (StableExpName        _ sn2) = compareSN sn1 sn2
  (StablePreExpName     _ sn1) == (StablePreExpName     _ sn2) = compareStableNamePreExp sn1 sn2
  _                          == _                          = False

compareSN :: (Typeable a, Typeable b) => StableName a -> StableName b -> Bool
compareSN sn1 sn2
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise                = False

--
-- There are no standard functions in Data.Typeable to handle casting of
-- a function of type t (a :: * -> *) (b :: * -> *) c.
--
-- Had to write our own.
--
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


