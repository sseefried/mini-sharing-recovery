{-# LANGUAGE GADTs, ScopedTypeVariables, FlexibleContexts, PatternGuards #-}
module Sharing where

-- Standard libraries
import Data.Typeable
import Control.Monad
import Control.Monad.State
import Prelude hiding (exp)
import Control.Applicative hiding (Const)
import Data.HashTable               as Hash
import qualified Data.IntMap        as IntMap
import System.Mem.StableName
import Data.Hashable
import Text.Printf
import System.IO.Unsafe
import Data.IORef

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Maybe
import Data.List

-- friends
import Debug
import HOAS


data StableExpName where
  StableExpName :: Typeable a => StableName (Exp a) -> StableExpName

instance Show StableExpName where
  show (StableExpName sn) = show $ hashStableName sn

instance Eq StableExpName where
  StableExpName sn1 == StableExpName sn2
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

instance Hashable StableExpName where
  hash (StableExpName sn) = hashStableName sn

makeStableExp :: Exp a-> IO (StableName (Exp a))
makeStableExp acc = acc `seq` makeStableName acc

-- Interleave sharing annotations into an expression AST.  Subtrees can be marked as being
-- represented by variable (binding a shared subtree) using 'VarSharing' and as being prefixed by
-- a let binding (for a shared subtree) using 'LetSharing'.
--
data SharingExp a where
  VarSharing :: Elt a => StableName (Exp a)                           -> SharingExp a
  LetSharing ::          StableSharingExp -> SharingExp a             -> SharingExp a
  ExpSharing :: Elt a => StableName (Exp a) -> PreExp SharingExp a -> SharingExp a

-- Stable name for an array computation associated with its sharing-annotated version.
--
data StableSharingExp where
  StableSharingExp :: Elt a => StableName (Exp a) -> SharingExp a -> StableSharingExp

instance Show StableSharingExp where
  show (StableSharingExp sn _) = show $ hashStableName sn

instance Eq StableSharingExp where
  StableSharingExp sn1 _ == StableSharingExp sn2 _
    | Just sn1' <- gcast sn1 = sn1' == sn2
    | otherwise              = False

-- Test whether the given stable names matches an array computation with sharing.
--
matchStableExp :: Elt a => StableName (Exp a) -> StableSharingExp -> Bool
matchStableExp sn1 (StableSharingExp sn2 _)
  | Just sn1' <- gcast sn1 = sn1' == sn2
  | otherwise              = False

-- Hash table keyed on the stable names of array computations.
--
type ExpHashTable v = Hash.HashTable StableExpName v

-- Mutable version of the occurrence map, which associates each AST node with an occurence count.
--
type OccMapHash = ExpHashTable Int

-- Create a new hash table keyed by array computations.
--
newExpHashTable :: IO (ExpHashTable v)
newExpHashTable = Hash.new (==) hashStableExp
  where
    hashStableExp (StableExpName sn) = fromIntegral (hashStableName sn)

-- Immutable version of the occurence map.  We use the 'StableName' hash to index an 'IntMap' and
-- disambiguate 'StableName's with identical hashes explicitly, storing them in a list in the
-- 'IntMap'.
--
type OccMap = IntMap.IntMap [(StableExpName, Int)]


-- Turn a mutable into an immutable occurence map.
--
freezeOccMap :: OccMapHash -> IO OccMap
freezeOccMap oc
  = do
      kvsOccMap <- Hash.toList oc
      return . IntMap.fromList . map (\kvs -> (key (head kvs), kvs)). groupBy sameKey $ kvsOccMap
  where
    key (StableExpName sn, _) = hashStableName sn
    sameKey kv1 kv2           = key kv1 == key kv2

-- Look up the occurence map keyed by array computations using a stable name.  If a the key does
-- not exist in the map, return an occurence count of '1'.
--
lookupWithExpName :: OccMap -> StableExpName -> Int
lookupWithExpName oc sa@(StableExpName sn)
  = fromMaybe 1 $ IntMap.lookup (hashStableName sn) oc >>= Prelude.lookup sa

-- Look up the occurence map keyed by array computations using a sharing array computation.  If an
-- the key does not exist in the map, return an occurence count of '1'.
--
lookupWithSharingExp :: OccMap -> StableSharingExp -> Int
lookupWithSharingExp oc (StableSharingExp sn _) = lookupWithExpName oc (StableExpName sn)


-- Compute the occurence map, marks all nodes with stable names, and drop repeated occurences
-- of shared subtrees (Phase One).
--
-- Note [Traversing functions and side effects]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- We need to descent into function bodies to build the 'OccMap' with all occurences in the
-- function bodies.  Due to the side effects in the construction of the occurence map and, more
-- importantly, the dependence of the second phase on /global/ occurence information, we may
-- not delay the body traversals by putting them under a lambda.  Hence, we apply each function
-- to a /unique identifier/ and then traverse its body.
--
-- We then wrap the result in the 'Tlam' constructor (which means 'tagged lambda body') along
-- with the unique identifier. During conversion to de Bruijn form we find 'Tag's with this
-- unique identifier and map them to the correct de Bruijn index. See function
-- 'convertSharingExp' in module Convert
--
makeOccMap :: Typeable a => Exp a -> IO (SharingExp a, OccMapHash)
makeOccMap rootExp
  = do
      uniques <- newIORef 0
      occMap <- newExpHashTable
      rootExp' <- traverseExp uniques True (enterOcc occMap) rootExp
      return (rootExp', occMap)
  where
    -- Enter one AST node occurrence into an occurrence map.  Returns 'True' if this is a repeated
    -- occurence.
    --
    -- The first argument determines whether the 'OccMap' will be modified - see Note [Traversing
    -- functions and side effects].
    --
    enterOcc :: OccMapHash -> Bool -> StableExpName -> IO Bool
    enterOcc occMap updateMap sa
      = do
          entry <- Hash.lookup occMap sa
          case entry of
            Nothing -> do when updateMap (       Hash.insert occMap sa 1      )
                          return False
            Just n  -> do when updateMap (void $ Hash.update occMap sa (n + 1))
                          return True
              where
                void = (>> return ())

    traverseExp :: forall a. Typeable a
                => IORef Int -> Bool -> (Bool -> StableExpName -> IO Bool) -> Exp a
                -> IO (SharingExp a)
    traverseExp uniques updateMap enter acc'@(Exp pexp)
      = do
            -- Compute stable name and enter it into the occurence map
          sn <- makeStableExp acc'
          isRepeatedOccurence <- enter updateMap $ StableExpName sn

          traceLine (showOp pexp) $
            if isRepeatedOccurence
              then "REPEATED occurence"
              else "first occurence (" ++ show (hashStableName sn) ++ ")"

            -- Reconstruct the computation in shared form
            --
            -- NB: This function can only be used in the case alternatives below; outside of the
            --     case we cannot discharge the 'Elt a' constraint.
          let reconstruct :: Elt a
                          => IO (PreExp SharingExp a)
                          -> IO (SharingExp a)
              reconstruct newExp | isRepeatedOccurence = pure $ VarSharing sn
                                 | otherwise           = ExpSharing sn <$> newExp

          case pexp of
            Tag i       -> reconstruct $ return $ Tag i
            App fun arg -> reconstruct $ do
                              fun' <- travF fun
                              arg' <- travE arg
                              return (App fun' arg')
            Const a     -> reconstruct $ return $ Const a
            Add a b     -> reconstruct $ do
                             a' <- travE a
                             b' <- travE b
                             return (Add a' b')
            Cond c t e  -> reconstruct $ do
                             c' <- travE c
                             t' <- travE t
                             e' <- travE e
                             return (Cond c' t' e')
            Eq a b      -> reconstruct $ do
                             a' <- travE a
                             b' <- travE b
                             return (Eq a' b')
      where
        travF :: (PreFun Exp b) -> IO (PreFun SharingExp b)
        travF = traverseFun uniques updateMap enter

        travE :: Typeable b => Exp b -> IO (SharingExp b)
        travE = traverseExp uniques updateMap enter
    -- See Note on [Traversing functions and side effects]
    traverseFun :: IORef Int -> Bool -> (Bool -> StableExpName -> IO Bool)
                -> (PreFun Exp b) -> IO (PreFun SharingExp b)
    traverseFun uniques updateMap enter (Lam f) = do
      unique <- readIORef uniques
      writeIORef uniques (unique + 1)
      body <- traverseExp uniques updateMap enter $ f (Exp $ Tag unique)

      return $ Tlam unique body

--
-- 'NodeCounts' is a type used to maintain how often each shared subterm occured and the
-- dependencies between shared subterms.
--
-- During phase 2 of the algorithm, when a shared subterm is replaced with a 'VarSharing'
-- node it may already contain another 'VarSharing' node. In this case it is said to have
-- a dependency on it. The 'DepGroup' data type captures these dependencies.
--
-- Invariant (for DepGroup): If one shared term 's' is itself a subterm of another shared
-- term 't', then edge '(t,s)' must appear in the DepGroup.
--
-- Invariant (for NodeCounts): The 'DepGroup's do not overlap. That is, for any two
-- 'DepGroup's, 'dg1' and 'dg2' the intersection of the nodes of dg1 and dg2 is empty.
-- The order of DepGroups themselves does not matter.
--
--
-- To ensure the invariant is preserved over merging node counts from sibling subterms,
-- the function '(+++)' must be used.
--

-- We use the 'HashMap' and 'HashSet' data structure of the 'unordered-containers' package
-- because 'StableExpName' does not have an 'Ord' instance.
data DepGroup = DepGroup { nodes :: HashMap StableExpName (StableSharingExp, Int)
                         , edges :: HashMap StableExpName (HashSet StableExpName) }

-- Nicer output for debugging.
instance Show DepGroup where
  show dg = printf "DepGroup { nodes = %s, edges = [%s] }"
              (show (map snd $ Map.toList (nodes dg)))
              (showNodes (Map.toList $ edges dg))
    where
      showNodes []     = ""
      showNodes [s]    = showNodes1 s
      showNodes (s:ss) = printf "%s, %s" (showNodes1 s) (showNodes ss)
      showNodes1 :: (StableExpName, HashSet StableExpName) -> String
      showNodes1 (sa,set) = printf "(%s, %s)" (show sa) (show (Set.toList set))

newtype NodeCounts = NodeCounts [DepGroup] deriving Show

emptyDepGroup :: DepGroup
emptyDepGroup = DepGroup { nodes = Map.empty, edges = Map.empty }

--
-- Merges two dependency groups.
--
mergeDepGroup :: DepGroup -> DepGroup -> DepGroup
mergeDepGroup dg1 dg2 = DepGroup newNodes newEdges
  where
    newNodes = Map.foldlWithKey' (\m k v -> Map.insertWith updateCount k v m)
                                 (nodes dg1) (nodes dg2)
    updateCount (sa1, count1) (sa2, count2) = (sa1 `pickNoneVar` sa2, count1 + count2)
    newEdges = Map.foldlWithKey' (\m k v -> Map.insertWith Set.union k v m)
                                 (edges dg1) (edges dg2)


depGroupInsertNode :: StableSharingExp -> DepGroup -> DepGroup
depGroupInsertNode sa dg = dg { nodes = newNodes }
  where
    san = stableExpNameOf sa
    newNode = case Map.lookup san (nodes dg) of
                 Just (sa', count) -> (sa `pickNoneVar` sa', 1 + count)
                 Nothing            -> (sa, 1)
    newNodes = Map.insert san newNode (nodes dg)

-- Precondition: The node must already be a member
depGroupInsertEdge :: StableSharingExp -> StableExpName -> DepGroup -> DepGroup
depGroupInsertEdge src tgtSA dg = dg { edges = newEdges }
  where
    srcSA    = stableExpNameOf src
    newEdges = Map.insertWith Set.union srcSA (Set.singleton tgtSA) (edges dg)

pickNoneVar :: StableSharingExp -> StableSharingExp -> StableSharingExp
(StableSharingExp _ (VarSharing _)) `pickNoneVar` sa2                                 = sa2
sa1                                 `pickNoneVar` _sa2                                = sa1

stableExpNameOf :: StableSharingExp -> StableExpName
stableExpNameOf (StableSharingExp sn _) = StableExpName sn

-- Empty node counts
--
noNodeCounts :: NodeCounts
noNodeCounts = NodeCounts []

-- Singleton node counts
--
-- Merges all the 'DepGroup's in 'subCounts', add the node 'stableSharingExp' with a
-- sharing count of 1, and also adds edges from this node to all the nodes in the merged
-- dependency group.
--
nodeCount :: StableSharingExp -> NodeCounts -> NodeCounts
nodeCount stableSharingExp (NodeCounts subCounts) =
  NodeCounts $ [depGroup]
  where
    mergedDepGroup :: DepGroup
    mergedDepGroup = foldl mergeDepGroup emptyDepGroup subCounts
    depGroup :: DepGroup
    -- Adds an edge for each node in 'mergedDepGroup'. This is probably
    -- overkill. It would only be necessary to add an edge to the "root" node
    -- of each 'DepGroup' in 'subCounts'.
    depGroup = Set.foldr (depGroupInsertEdge stableSharingExp)
                         (depGroupInsertNode stableSharingExp mergedDepGroup)
                         (keysSet (nodes mergedDepGroup))

--
-- Combine two NodeCounts. Any 'DepGroup's that overlap are combined
--
(+++) :: NodeCounts -> NodeCounts -> NodeCounts
NodeCounts us +++ NodeCounts vs = NodeCounts $ tracedMerge
  where
    tracedMerge
      | length us > 0 && length vs > 0 = trace debugMsg result
      | otherwise                      = result
      where
        result = merge us vs
        debugMsg = printf "     %s\n        `merge`\n     %s\n       ==\n     %s"
                     (show us) (show vs) (show result)
    merge :: [DepGroup] -> [DepGroup] -> [DepGroup]
    merge []                         ys                         = ys
    merge xs                         []                         = xs
    merge (xs:xss) yss                  = mergeInto xs (merge xss yss)

    mergeInto :: DepGroup -> [DepGroup] -> [DepGroup]
    mergeInto xs [] = [xs]
    mergeInto xs (ys:yss)
      | overlap xs ys = mergeInto (mergeDepGroup xs ys) yss
      | otherwise     = ys:mergeInto xs yss

    -- Note: This is quadratic in complexity. HashSet does not have an 'intersection' method.
    overlap :: DepGroup -> DepGroup -> Bool
    overlap dg1 dg2 = overlap' (Map.keys $ nodes dg1) (Map.keys $ nodes dg2)
      where
        overlap' []  _     = False
        overlap' (x:xs) ys = x `elem` ys || overlap' xs ys

keysSet :: (Eq k, Hashable k) => HashMap k a -> HashSet k
keysSet = Set.fromList . Map.keys

type TopoSortState = (HashSet StableExpName, [(StableSharingExp, Int)])

--
-- Returns the 'StableSharingExp's for a 'DepGroup' along with the sharing count
-- in reverse-binding order.
--
stableSharingExpsForDepGroup :: DepGroup -> [(StableSharingExp, Int)]
stableSharingExpsForDepGroup dg = topoSort (Map.keys $ nodes dg) Set.empty []
  where
   -- topological sort
   topoSort :: [StableExpName] -> HashSet StableExpName -> [(StableSharingExp, Int)]
            -> [(StableSharingExp, Int)]
   topoSort [] _ accum = accum
   topoSort (san:sans) visited accum =
     let (visited', result) = visit san (visited, accum)
     in topoSort sans visited' accum ++ result
   visit :: StableExpName -> TopoSortState -> TopoSortState
   visit san this@(visited, accum)
     | Set.member san visited = this
     | otherwise =
         let visited' = Set.insert san visited
             element  = fromJust $ Map.lookup san $ nodes dg
             (visited'', accum'') = case Map.lookup san (edges dg) of
                Just succs -> Set.foldr visit (visited', accum) succs
                Nothing -> (visited', accum)
         in (visited'', element : accum'')


--
-- Invariant. All values of type PreFun SharingExp a are constructed with Tlam not Lam
--
determineScopes :: Typeable a => OccMap -> SharingExp a -> SharingExp a
determineScopes occMap rootAcc = fst $ scopesExp rootAcc
  where
    scopesExp :: forall a. SharingExp a -> (SharingExp a, NodeCounts)
    scopesExp (LetSharing _ _) = error "determineScopes: scopes: unexpected 'LetSharing'"
    scopesExp sharingExp@(VarSharing sn) = trace debugMsg (VarSharing sn, newCount)
      where
        newCount = nodeCount (StableSharingExp sn sharingExp) noNodeCounts
        debugMsg = printf "%s: (VarSharing) %s" (show $ StableExpName sn) (show newCount)
    scopesExp (ExpSharing sn pexp) = case pexp of
      Tag i            -> reconstruct (Tag i) noNodeCounts
      App fun arg      -> let
                            (fun', accCount1) = scopesFun fun
                            (arg', accCount2) = scopesExp arg
                          in reconstruct (App fun' arg') (accCount1 +++ accCount2)
      Const v          -> reconstruct (Const v) noNodeCounts
      Add e1 e2        -> let
                            (e1', accCount1) = scopesExp e1
                            (e2', accCount2) = scopesExp e2
                          in reconstruct (Add e1' e2') (accCount1 +++ accCount2)
      Cond cnd thn els -> let
                            (cnd', accCount1) = scopesExp cnd
                            (thn', accCount2) = scopesExp thn
                            (els', accCount3) = scopesExp els
                          in reconstruct (Cond cnd' thn' els')
                               (accCount1 +++ accCount2 +++ accCount3)
      Eq e1 e2         -> let
                            (e1', accCount1) = scopesExp e1
                            (e2', accCount2) = scopesExp e2
                          in reconstruct (Eq e1' e2') (accCount1 +++ accCount2)
      where
        -- trav
        occCount = lookupWithExpName occMap (StableExpName sn)
        reconstruct :: Elt a => PreExp SharingExp a -> NodeCounts -> (SharingExp a, NodeCounts)
        reconstruct newExp subCount = trace debugMsg reconstruct'
          where
            debugMsg = printf ("%s: bindHere = %s\n" ++
                               "   subCount = %s\n" ++
                               "   newCount = %s\n" ++
                               "   newNodeCounts = %s")
                        (show $ StableExpName sn) (show $ bindHere) (show $ subCount)
                        (show $ newCount) (if occCount > 1 then show letBody else "---")
            letBody = nodeCount (StableSharingExp sn sharingExp) newCount
            reconstruct'
              | occCount > 1 = (VarSharing sn, letBody)
              | otherwise = (sharingExp, newCount)
            -- Determine the bindings that need to be attached to the current node...
            (newCount, bindHere) = filterCompleted subCount
            lets = foldl (flip (.)) id . map LetSharing $ bindHere
            sharingExp = lets $ ExpSharing sn newExp
        -- Extract nodes that have a complete node count (i.e., their node count is equal
        -- to the number of occurences of that node in the overall expression) => nodes
        -- with a completed node count should be let bound at the currently processed
        -- node.
        --
        -- Nodes are extracted on a per dependency group ('DepGroup') basis. If all the
        -- nodes in a dependency group have a sharing count equal to their occurrence
        -- counts then they are filtered out.
        --
        filterCompleted :: NodeCounts -> (NodeCounts, [StableSharingExp])
        filterCompleted (NodeCounts counts)
          = let (counts', completed) = fc counts
            in (NodeCounts counts', completed)
          where
            fc []                 = ([], [])
            fc (sub:subs)
                -- current node is the binding point for the shared node 'sa'
              | (True, sharingExps) <- readyToBind sub = (subs', sharingExps ++ bindHere)
                -- not a binding point
              | otherwise         = (sub:subs', bindHere)
              where
                (subs', bindHere) = fc subs
                -- Returns a boolean indicating whether the DepGroup is ready for binding
                -- and the things to be bound.
                readyToBind :: DepGroup -> (Bool, [StableSharingExp])
                readyToBind dg = (isReady, map fst toBind)
                  where
                    toBind = stableSharingExpsForDepGroup dg
                    isReady = all (\(sa,n) -> lookupWithSharingExp occMap sa == n) toBind
        -- The lambda bound variable is at this point already irrelevant; for details see
        -- Note [Traversing functions and side effects]
        scopesFun :: SharingFun b -> (SharingFun b, NodeCounts)
        scopesFun (Lam _) = error "Lam should not exist in AST after makeOccMap"
        scopesFun (Tlam n exp) = (Tlam n exp', counts)
          where
            (exp', counts) = scopesExp exp

type SharingFun t = PreFun SharingExp t

-- |Recover sharing information and annotate the HOAS AST with variable and let binding
--  annotations.  The first argument determines whether array computations are floated out of
--  expressions irrespective of whether they are shared or not — 'True' implies floating them out.
--
-- NB: Strictly speaking, this function is not deterministic, as it uses stable pointers to
--     determine the sharing of subterms.  The stable pointer API does not guarantee its
--     completeness; i.e., it may miss some equalities, which implies that we may fail to discover
--     some sharing.  However, sharing does not affect the denotational meaning of an array
--     computation; hence, we do not compromise denotational correctness.
--
recoverSharing :: Typeable a => Exp a -> SharingExp a
{-# NOINLINE recoverSharing #-}
recoverSharing expr =
    let (exp, occMap) =   -- as we need to use stable pointers; it's safe as explained above
            unsafePerformIO $ do
              (exp', occMap') <- makeOccMap expr
              occMapList <- Hash.toList occMap'
              traceChunk "OccMap" $ show occMapList
              frozenOccMap <- freezeOccMap occMap'
              return (exp', frozenOccMap)
    in determineScopes occMap exp

