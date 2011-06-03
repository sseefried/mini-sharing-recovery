{-# LANGUAGE GADTs  #-}
--
-- A module for pretty priting sharing recovering graphs
--
module Graphviz where

-- standard libraries
import Data.IORef
import Data.Typeable
import System.Mem.StableName

-- friends
import HOAS
import Sharing hiding (StableExpName)
import Graph

type EnterFun = (GraphStableName, GraphStableName) -> IO ()

dotSharingExp :: Typeable a => SharingExp a -> IO Graph
dotSharingExp rootSharingExp = do
  graphIORef <- newIORef newGraph
  src <- sharingExpStableName rootSharingExp
  traverseSharingExp (addEdge graphIORef) src rootSharingExp
  graph <- readIORef graphIORef
  return graph
  where
    addEdge :: IORef Graph -> EnterFun
    addEdge graphIORef edge = do
      graph <- readIORef graphIORef
      let graph' = insertEdge graph edge
      writeIORef graphIORef graph'
    traverseSharingExp :: Typeable a => EnterFun -> GraphStableName -> SharingExp a -> IO ()
    traverseSharingExp enter src sharingExp = case sharingExp of
      VarSharing _ -> return ()
      LetSharing (StableSharingExp _ sharingExp1) sharingExp2 -> do
        tgt1 <- sharingExpStableName sharingExp1
        tgt2 <- sharingExpStableName sharingExp2
        enter (src,tgt1)
        enter (src,tgt2)
        traverseSharingExp enter tgt1 sharingExp1
        traverseSharingExp enter tgt2 sharingExp2
      ExpSharing _ pexp -> do
         tgt <- preExpStableName pexp
         enter (src,tgt)
         traversePreExp enter tgt pexp

    traverseSharingFun :: Typeable a => EnterFun -> GraphStableName -> SharingFun a -> IO ()
    traverseSharingFun enter src sharingFun = case sharingFun of
      TaggedSharingExp _ sharingExp -> do
        tgt <- sharingExpStableName sharingExp
        enter (src, tgt)
        traverseSharingExp enter tgt sharingExp
    traversePreExp :: Typeable a => EnterFun -> GraphStableName
                   -> PreExp SharingExp SharingFun a -> IO ()
    traversePreExp enter src pexp = case pexp of
      Tag _ -> return ()
      App fun arg -> do
        tgt1 <- sharingFunStableName fun
        tgt2 <- sharingExpStableName arg
        enter (src, tgt1)
        enter (src, tgt2)
        traverseSharingFun enter tgt1 fun
        traverseSharingExp enter tgt2 arg
      Const _ -> return ()
      Add e1 e2 -> travE2 e1 e2
      Cond cnd thn els -> do
        tgt1 <- sharingExpStableName cnd
        tgt2 <- sharingExpStableName thn
        tgt3 <- sharingExpStableName els
        enter (src, tgt1)
        enter (src, tgt2)
        enter (src, tgt3)
        traverseSharingExp enter tgt1 cnd
        traverseSharingExp enter tgt2 thn
        traverseSharingExp enter tgt3 els
      Eq e1 e2 -> travE2 e1 e2
      where
        travE2 e1 e2 = do
          tgt1 <- sharingExpStableName e1
          tgt2 <- sharingExpStableName e2
          enter (src, tgt1)
          enter (src, tgt2)
          traverseSharingExp enter tgt1 e1
          traverseSharingExp enter tgt2 e2

sharingExpStableName :: Typeable a => SharingExp a -> IO GraphStableName
sharingExpStableName sharingExp = do
  sn <- makeStableName sharingExp
  return (StableSharingExpName (showSharingExpOp sharingExp) sn)

sharingFunStableName :: Typeable a => SharingFun a -> IO GraphStableName
sharingFunStableName sharingFun = do
  sn <- makeStableName sharingFun
  return (StableSharingFunName (showSharingFunOp sharingFun) sn)

preExpStableName :: Typeable a => PreExp SharingExp SharingFun a -> IO GraphStableName
preExpStableName pexp = do
  sn <- makeStableName pexp
  return (StablePreExpName (showPreExpOp pexp) sn)