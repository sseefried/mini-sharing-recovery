{-# LANGUAGE GADTs, PatternGuards, RankNTypes, ScopedTypeVariables, KindSignatures #-}
module Graph (
  GraphStableName(..),
  Graph, -- opaque
  -- * Functions
  newGraph, insertEdge

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

-- friends
import HOAS
import Sharing hiding (StableExpName)

data GraphNodeType = SharingExpType | ExpType | FunType | SharingFunType | PreExpType
   deriving (Eq, Show, Ord)

-- By pairing the hash of the stable name and the position in the lookup list we
-- it allows us to have an Ord instance.
data GraphNode = GraphNode
                   GraphNodeType  -- type
                   Int            -- hash of stable name
                   Int            -- position in lookup list
   deriving (Eq, Show, Ord)

data GraphStableName  where
  StableSharingExpName :: Typeable a => StableName (SharingExp a)     -> GraphStableName
  StableSharingFunName :: Typeable a => StableName (SharingFun a)     -> GraphStableName
  StableExpName        :: Typeable a => StableName (Exp a)            -> GraphStableName
  StableFunName        :: Typeable a => StableName (Fun a)            -> GraphStableName
  StablePreExpName     :: (Typeable a, Typeable1 exp, Typeable1 fun)
                       => StableName (PreExp exp fun a) -> GraphStableName

--
-- Graphs
--

type GraphLookup = IntMap [GraphStableName]
data Graph = Graph { graphLookup :: GraphLookup
                   , graphNodes :: Set GraphNode
                   , graphEdges :: Map GraphNode GraphNode }

newGraph :: Graph
newGraph = Graph { graphLookup = IntMap.empty, graphNodes = Set.empty, graphEdges = Map.empty }

insertEdge :: Graph -> (GraphStableName, GraphStableName) -> Graph
insertEdge g (src, tgt) = g { graphNodes = graphNodes', graphEdges = graphEdges' }
  where
    graphLookup' = foldl insertGraphLookup (graphLookup g) [src, tgt]
    srcNode = fromJust $ lookupGraphNode src graphLookup'
    tgtNode = fromJust $ lookupGraphNode tgt graphLookup'
    graphNodes' = foldr Set.insert (graphNodes g) [srcNode, tgtNode]
    graphEdges' = Map.insert srcNode tgtNode (graphEdges g)

insertGraphLookup :: GraphLookup -> GraphStableName -> GraphLookup
insertGraphLookup tbl gsn = IntMap.insert hash (gsns ++ [gsn]) tbl
  where
    hash = hashGraphStableName gsn
    gsns = fromMaybe [] $ IntMap.lookup hash tbl

hashGraphStableName :: GraphStableName -> Int
hashGraphStableName gsn = case gsn of
  StableSharingExpName sn -> hashStableName sn
  StableSharingFunName sn -> hashStableName sn
  StableExpName        sn -> hashStableName sn
  StableFunName        sn -> hashStableName sn
  StablePreExpName     sn -> hashStableName sn

--
-- Look up the occurence map keyed by array computations using a stable name.  If a the key does
-- not exist in the map, return an occurence count of '1'.
--
lookupGraphNode :: GraphStableName -> GraphLookup -> Maybe GraphNode
lookupGraphNode sa@(StableSharingExpName sn) = lookupGraphNodeAux SharingExpType sn sa
lookupGraphNode sa@(StableSharingFunName sn) = lookupGraphNodeAux SharingFunType sn sa
lookupGraphNode sa@(StableExpName sn)        = lookupGraphNodeAux ExpType        sn sa
lookupGraphNode sa@(StableFunName sn)        = lookupGraphNodeAux FunType        sn sa
lookupGraphNode sa@(StablePreExpName sn)     = lookupGraphNodeAux PreExpType     sn sa

lookupGraphNodeAux :: GraphNodeType -> StableName a -> GraphStableName
                   -> GraphLookup -> Maybe GraphNode
lookupGraphNodeAux gnType sn sa nds = do
  let hash = hashStableName sn
  gsns <- IntMap.lookup hash nds
  i   <- findIndex (==sa) gsns
  return (GraphNode gnType hash i)

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


