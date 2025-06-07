module Algebra.History where

import Domain.DomainTypes
import Numeric.Natural
import Algebra.HelperFunctions

import Data.Tree



joinTrees :: HistoryTree -> HistoryTree -> HistoryTree
joinTrees (Node label []) newchild = Node label [newchild]
joinTrees (Node label (t:ts)) newchild =
  Node label (joinTrees t newchild : ts)

getParentTree :: Natural -> HistoryTree -> HistoryTree
getParentTree _ (Node parent []) = Node parent []
getParentTree 0 (Node parent children) = Node parent []
getParentTree 1 (Node parent children) = Node parent []
getParentTree pos (Node parent children) = Node parent [getParentTree (pos-1) ch]
    where 
        [ch] = safehead children

getCurrentTree :: Natural -> HistoryTree -> HistoryTree
getCurrentTree _ (Node parent []) = Node parent []
getCurrentTree 0 (Node parent children) = Node parent children
getCurrentTree 1 (Node parent children) = ch
    where 
        [ch] = safehead children
getCurrentTree pos (Node parent children) = getCurrentTree (pos-1) ch
    where 
        [ch] = safehead children


getBufferFromTree :: Natural -> HistoryTree -> CurrentBuffer
getBufferFromTree pos tree = cb
    where 
        (Node cb children) = getCurrentTree pos tree


addToTree :: Natural -> CurrentBuffer -> HistoryTree -> HistoryTree
addToTree  _ cb (Node parent []) = Node parent [Node cb []]
addToTree  0 cb (Node parent children) = Node parent (Node cb [] : children)
addToTree  pos cb tree = joinTrees parenttree newchild
    where 
        parenttree = getParentTree pos tree
        nexttree = getCurrentTree pos tree
        newchild = addToTree 0 cb nexttree

addNewToTree :: Natural -> CurrentBuffer -> HistoryTree -> (HistoryTree, Natural)
addNewToTree pos cb tree = 
    if isdifferent 
        then (addToTree pos cb tree, pos+1)
        else (tree, pos)
    where 
        isdifferent = cb /= getBufferFromTree pos tree

getMaxPosTree ::  HistoryTree -> Natural
getMaxPosTree  (Node parent []) = 0
getMaxPosTree (Node parent children) = 1 + getMaxPosTree ch 
    where 
        [ch] = safehead children

