{-# LANGUAGE GADTs  #-}
--
-- A module for pretty priting sharing recovering graphs
--
module Graphviz where

-- friends
import Sharing hiding (StableExpName)
import Graph






dotSharingExp :: SharingExp a -> IO Graph
dotSharingExp sharingExp = do
  let graph = newGraph
  return graph