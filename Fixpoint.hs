module Fixpoint where

import Domain
import Program
import Worklist
import Semantics

import LLVM.AST.Global as LLVMIR

getParamName :: LLVMIR.Parameter -> String
getParamName (LLVMIR.Parameter _ name _) = show name

  
inputOf :: Node -> CFG -> Table -> State
inputOf here cfg table =
  case isEntry cfg here of
    True  -> foldl (\acc param ->
                      bindState (getParamName param) Top acc) emptyState (getParams cfg)
    False -> foldl (\acc param ->
                      let res = findTable param table in
                      stateJoin res acc) emptyState (predOfBlock cfg here)

needWiden :: Node -> Bool
needWiden _ = True


-- | The core worklist algorithm.
analyzeInner :: CFG -> Table -> Worklist -> Table
analyzeInner _ table [] = table
analyzeInner cfg table wklist =
  let (here, wklist') = pop wklist
      state           = inputOf here cfg table
      state'          = transferBlock state (getInstrs here)
      oldState        = findTable here table in
    if not $ stateOrder state' oldState
    then let table'   = if needWiden here
                        then bindTable here (stateWiden oldState state') table
                        else bindTable here (stateJoin oldState state') table
             wklist'' = addSet wklist' (succOfBlock cfg here) in
           (analyzeInner cfg table' wklist'')
    else (analyzeInner cfg table wklist')


analyze :: CFG -> Table
analyze cfg =
  let wklist = addSet newWorklist (getBlocks cfg) in
    analyzeInner cfg newTable wklist
