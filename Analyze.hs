module Analyze where

import Program
import Domain
import Worklist
import Data.Map (lookup)

import LLVM.AST.Instruction
import LLVM.AST.Type
import LLVM.AST.Global
import LLVM.AST.Operand
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate
import Data.Foldable (foldl')

evalArgument :: Operand -> State -> Interval
evalArgument operand state =
  case operand of
    ConstantOperand (Int _ intVal) -> interFromInt $ fromIntegral intVal
    LocalReference _ name -> findState (show name) state


transferInstAdd :: State -> Named Instruction -> State
transferInstAdd state (name := LLVM.AST.Instruction.Add _ _ op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  bindState (show name) (interPlus op0Inter op1Inter) state
  

transferInstSub :: State -> Named Instruction -> State
transferInstSub state (name := LLVM.AST.Instruction.Sub _ _ op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  bindState (show name) (interMinus op0Inter op1Inter) state


transferInstMul :: State -> Named Instruction -> State
transferInstMul state (name := LLVM.AST.Instruction.Mul _ _ op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  bindState (show name) (interMult op0Inter op1Inter) state


transferInstICmp :: State -> Named Instruction -> State
transferInstICmp state (name := LLVM.AST.Instruction.ICmp iPred op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  let itv = (case iPred of
                SLE   -> interSLE op0Inter op1Inter
                iPred -> Top) in
    bindState (show name) (interSLE op0Inter op1Inter) state


transferInstPhi :: State -> Named Instruction -> State
transferInstPhi state (name := LLVM.AST.Instruction.Phi _ inVals _) =
  let inVals_ = map (\(x, y) -> x) inVals
      inter    = foldl' (\acc elem -> interJoin acc (evalArgument elem state)) Bot inVals_ in
    bindState (show name) inter state


transferInstCall :: State -> Named Instruction -> State
transferInstCall state (name := LLVM.AST.Instruction.Call _ _ _ callee args _ _) =
  undefined


transferInst :: State -> Named Instruction -> State
transferInst state instr = case instr of
  _ := LLVM.AST.Instruction.Add _ _ _ _ _      -> transferInstAdd state instr
  _ := LLVM.AST.Instruction.Sub _ _ _ _ _      -> transferInstSub state instr
  _ := LLVM.AST.Instruction.Mul _ _ _ _ _      -> transferInstMul state instr
  _ := LLVM.AST.Instruction.ICmp _ _ _ _       -> transferInstICmp state instr
  _ := LLVM.AST.Instruction.Phi _ _ _          -> transferInstPhi state instr
  _ := LLVM.AST.Instruction.Call _ _ _ _ _ _ _ -> transferInstCall state instr
  _ -> state


-- | Transfer a whole block.
transferBlock :: State -> [Named Instruction] -> State
transferBlock state instrs = foldl' (\acc instr -> transferInst acc instr) state instrs


getParamName :: Parameter -> String
getParamName (Parameter _ name _) = show name

  
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
