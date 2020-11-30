module Analyze where

import Program
import Domain
import Worklist
import Data.Map (lookup)

import LLVM.AST.Instruction as LLVMIR
import LLVM.AST.Type
import LLVM.AST.Global
import LLVM.AST.Operand as LLVMIR
import LLVM.AST.Float as LLVMIR
import LLVM.AST.Constant as Constant
import LLVM.AST.IntegerPredicate
import LLVM.AST.Name
import Data.Foldable (foldl')

evalArgument :: Operand -> State -> Interval
evalArgument operand state =
  case operand of
    ConstantOperand (Int _ intVal) -> interFromInt $ fromIntegral intVal
    LocalReference _ name -> findState (show name) state


transferInstAdd :: State -> Named Instruction -> State
transferInstAdd state (name := LLVMIR.Add _ _ op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  bindState (show name) (interPlus op0Inter op1Inter) state
  

transferInstSub :: State -> Named Instruction -> State
transferInstSub state (name := LLVMIR.Sub _ _ op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  bindState (show name) (interMinus op0Inter op1Inter) state


transferInstMul :: State -> Named Instruction -> State
transferInstMul state (name := LLVMIR.Mul _ _ op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  bindState (show name) (interMult op0Inter op1Inter) state


transferInstICmp :: State -> Named Instruction -> State
transferInstICmp state (name := LLVMIR.ICmp iPred op0 op1 _) =
  let op0Inter = evalArgument op0 state
      op1Inter = evalArgument op1 state in
  let itv = (case iPred of
                SLE   -> interSLE op0Inter op1Inter
                iPred -> Top) in
    bindState (show name) (interSLE op0Inter op1Inter) state


transferInstPhi :: State -> Named Instruction -> State
transferInstPhi state (name := LLVMIR.Phi _ inVals _) =
  let inVals_ = map (\(x, y) -> x) inVals
      inter    = foldl' (\acc elem -> interJoin acc (evalArgument elem state)) Bot inVals_ in
    bindState (show name) inter state


evalConst :: Constant -> Interval
evalConst (Constant.Int _ intVal)    = interFromInteger intVal
evalConst (Constant.Float floatVal)  =
  case floatVal of
    LLVMIR.Single float  -> interFromFloat float
    LLVMIR.Double double -> interFromFloat (realToFrac double)
    sth                  -> error $ "not a supported real number: " ++ (show sth)
evalConst (Constant.Add _ _ op0 op1) = interPlus (evalConst op0) (evalConst op1)
evalConst (Constant.Sub _ _ op0 op1) = interMinus (evalConst op0) (evalConst op1)
evalConst (Constant.Mul _ _ op0 op1) = interMult (evalConst op0) (evalConst op1)
evalConst _                 = error "non-Int/non-Float arg value not supported"


transferInstCall :: [CFG] -> State -> Named Instruction -> State
transferInstCall cfgPool state (name := LLVMIR.Call _ _ _ callee args _ _) =
  let args'       = map (\(x, y) -> x) args
      argIntvs    = map (\arg ->
                          case arg of
                            LocalReference _ name -> lookupWithExn (show name) state -- show name이 맞으려나?
                            ConstantOperand const -> evalConst const) args'
      argAndIntvs = zip args' argIntvs
      calleeCFG   = findCFGByName callee cfgPool
      calleeTbl   = analyze argAndIntvs cfgPool calleeCFG 
      retBlock    = findRetBlock calleeCFG
      retState    = lookupWithExn retBlock calleeTbl
      retInstr    = getTerminator retBlock
      retOperand  = findRetOperand retInstr
      retIntv     = lookupWithExn retOperand retState in
    bindState (show name) retIntv state
transferInstCall _ state (Do (LLVMIR.Call _ _ _ _ _ _ _)) = state


-- | Does the Block end with Return?
hasRet :: Node -> Bool
hasRet (BasicBlock _ _ terminator) =
  case terminator of
    Do (Ret _ _) -> True
    _            -> False


-- | Find all blocks ending with Return
findRetBlock :: CFG -> Node
findRetBlock cfg =
  let nodePool  = blocks cfg
      retBlocks = foldl (\acc node ->
                           if hasRet node
                           then node:acc
                           else acc) [] nodePool in
    case retBlocks of
      []     -> error "there are no return blocks; this is impossible."
      [node] -> node
      _      -> error "there cannot be multiple return blocks; this is impossible."


findRetOperand :: Terminator -> String
findRetOperand (Ret operand _) = show operand


findCFGByName :: LLVMIR.CallableOperand -> [CFG] -> CFG
findCFGByName (Left _) _ = error "Inline assembly not supported"
findCFGByName (Right operand) cfgPool =
  case operand of
    LocalReference _ name ->
      let matches = foldl (\acc cfg ->
                                if fid cfg == (show name) then cfg:acc else acc) [] cfgPool in
        case matches of
          []     -> error "could not find such CFG"
          [cfg]  -> cfg
          (x:xs) -> error "multiple CFGs exist with that name"
    ConstantOperand _     -> error "Constant callable operand not supported"
    MetadataOperand _     -> error "Metadata callable operand not supported"


transferInst :: [CFG] -> State -> Named Instruction -> State
transferInst cfgPool state instr = case instr of
  _ := LLVMIR.Add _ _ _ _ _      -> transferInstAdd state instr
  _ := LLVMIR.Sub _ _ _ _ _      -> transferInstSub state instr
  _ := LLVMIR.Mul _ _ _ _ _      -> transferInstMul state instr
  _ := LLVMIR.ICmp _ _ _ _       -> transferInstICmp state instr
  _ := LLVMIR.Phi _ _ _          -> transferInstPhi state instr
  _ := LLVMIR.Call _ _ _ _ _ _ _ -> transferInstCall cfgPool state instr
  _ -> state


-- | Transfer a whole block.
transferBlock :: [CFG] -> State -> [Named Instruction] -> State
transferBlock cfgPool state instrs = foldl' (\acc instr -> transferInst cfgPool acc instr) state instrs


getParamName :: Parameter -> String
getParamName (Parameter _ name _) = show name

  
inputOf :: Node -> CFG -> Table -> State
inputOf here cfg table =
  case isEntry cfg here of
    True  -> foldl (\acc param ->
                      bindState (getParamName param) Top acc) emptyState (getParams cfg)
    False -> foldl (\acc param ->
                      let res = findTable param table [] in
                      stateJoin res acc) emptyState (predOfBlock cfg here)


needWiden :: Node -> Bool
needWiden _ = True


-- | The core worklist algorithm.
analyzeInner :: [(Operand, Interval)] -> [CFG] -> CFG -> Table -> Worklist -> Table
analyzeInner _ _ _ table [] = table
analyzeInner argAndItvs cfgPool cfg table wklist =
  let (here, wklist') = pop wklist
      state           = inputOf here cfg table
      state'          = transferBlock cfgPool state (getInstrs here)
      oldState        = findTable here table argAndItvs in
    if not $ stateOrder state' oldState
    then let table'   = if needWiden here
                        then bindTable here (stateWiden oldState state') table
                        else bindTable here (stateJoin oldState state') table
             wklist'' = addSet wklist' (succOfBlock cfg here) in
           (analyzeInner argAndItvs cfgPool cfg table' wklist'')
    else (analyzeInner argAndItvs cfgPool cfg table wklist')


analyze :: [(Operand, Interval)] -> [CFG] -> CFG -> Table
analyze argAndItvs cfgPool cfg =
  let wklist = addSet newWorklist (getBlocks cfg) in
    analyzeInner argAndItvs cfgPool cfg newTable wklist
