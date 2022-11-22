module Stackctl.Sort
  ( sortByDependencies
  ) where

import Stackctl.Prelude

import Data.Graph (graphFromEdges, topSort)

sortByDependencies
  :: Ord k
  => (a -> k)
  -- ^ How to identify a given item
  -> (a -> [k])
  -- ^ How to get the given item's dependencies
  -> [a]
  -> [a]
sortByDependencies toName toDepends specs =
  map nodeFromVertex $ reverse $ topSort graph
 where
  (graph, tripleFromVertex, _) = graphFromEdges $ map tripleFromNode specs

  nodeFromVertex = nodeFromTriple . tripleFromVertex

  tripleFromNode n = (n, toName n, toDepends n)

  nodeFromTriple (n, _, _) = n
