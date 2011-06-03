{-# LANGUAGE GADTs  #-}
--
-- A module for pretty priting sharing recovering graphs
--
module Graphviz where

-- standard libraries
import Data.IORef
import Data.Typeable
import System.Mem.StableName
import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath
import System.IO
import System.IO.Error hiding (catch)
import System.Directory
import System.Posix.Process
import System.Exit
import Control.Exception (finally)

-- friends
import HOAS
import Sharing hiding (StableExpName)
import Graph

type EnterFun = (GraphStableName, GraphStableName) -> IO ()

sharingExpToGraph :: Typeable a => SharingExp a -> IO Graph
sharingExpToGraph rootSharingExp = do
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
  let name = printf "%s" (showSharingExpOp sharingExp)
  return (StableSharingExpName name sn)

sharingFunStableName :: Typeable a => SharingFun a -> IO GraphStableName
sharingFunStableName sharingFun = do
  sn <- makeStableName sharingFun
  let name = printf "%s" (showSharingFunOp sharingFun)
  return (StableSharingFunName name sn)

preExpStableName :: Typeable a => PreExp SharingExp SharingFun a -> IO GraphStableName
preExpStableName pexp = do
  sn <- makeStableName pexp
  let name = printf "%s" (showPreExpOp pexp)
  return (StablePreExpName name sn)

toDot :: Graph -> [String]
toDot graph = "digraph G { " : dotNodes graph ++ dotEdges graph ++ ["}"]
  where
    dotEdges :: Graph -> [String]
    dotEdges g = Map.foldWithKey (\src tgts xs -> makeEdges src tgts ++ xs) [] (graphEdges g)
    makeEdges :: GraphNode -> Set GraphNode -> [String]
    makeEdges src tgts = map (makeEdge src) (Set.toList tgts)
    makeEdge :: GraphNode -> GraphNode -> String
    makeEdge src tgt = printf "%s -> %s;" (idForGraphNode src) (idForGraphNode tgt)
    dotNodes :: Graph -> [String]
    dotNodes g = map dotNode (Set.toList (graphNodes g))
    dotNode :: GraphNode -> String
    dotNode nd@(GraphNode typ nm _ _) = printf "%s [%s, label=\"%s\"];"
                                        (idForGraphNode nd) (dotShapeForType typ) nm

dotShapeForType :: GraphNodeType -> String
dotShapeForType gnt = case gnt of
  SharingExpType -> "shape=box"
  ExpType        -> "shape=ellipse"
  FunType        -> "shape=diamond"
  SharingFunType -> "shape=diamond"
  PreExpType     -> "shape=ellipse"

idForGraphNode :: GraphNode -> String
idForGraphNode (GraphNode _ _ hash pos) = printf "node_%02d_%02d" hash pos

dotSharingExp :: Typeable a => SharingExp a -> IO ()
dotSharingExp sharingExp = do
  g <- sharingExpToGraph sharingExp
  let dotString = unlines $ toDot g
  exists <- findExecutable "dot"
  case exists of
    Just dot -> withTempFile "test.dot" (writePSFile dot dotString)
    Nothing  -> do
      putStrLn "Couldn't find `dot' tool. Just writing DOT file."
      writeDotFile dotString
  where
    writePSFile dot dotString file h = do
      hPutStr h dotString
      hFlush h
      let output = "test" <.> "ps"
          flags  = [file, "-Tps", "-o" ++ output]
      status <- getProcessStatus True True =<< forkProcess (executeFile dot False flags Nothing)
      case status of
           Just (Exited ExitSuccess) -> putStrLn $ "PS file successfully written to `" ++
                                          output ++ "'"
           _                         -> do
             putStrLn "dot failed to write Postscript file. Just writing the DOT file."
             writeDotFile dotString       -- fall back to writing the dot file
    --
    writeDotFile :: String -> IO ()
    writeDotFile dotString = catch (writeDotFile' dotString) handler
    writeDotFile' dotString = do
      let path = "test" <.> "dot"
      h <- openFile path WriteMode
      hPutStr h dotString
      putStrLn ("DOT file successfully written to `" ++ path ++ "'")
      hClose h
    handler :: IOError -> IO ()
    handler e =
      case True of
        _ | isAlreadyInUseError e -> putStrLn "isAlreadyInUseError"
          | isDoesNotExistError e -> putStrLn "isDoesNotExistError"
          | isFullError e         -> putStrLn "isFullError"
          | isEOFError e          -> putStrLn "isEOFError"
          | isPermissionError   e -> putStrLn "isPermissionError"
          | isIllegalOperation e  -> putStrLn "isIllegalOperation"
          | isUserError e         -> putStrLn "isUserError"
          | otherwise             -> putStrLn "Unknown error"

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern f = do
  tempDir <- catch getTemporaryDirectory (\_ -> return ".")
  (tempFile, tempH) <- openTempFile tempDir pattern
  finally (f tempFile tempH) (hClose tempH >> removeFile tempFile)
