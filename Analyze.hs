module Analyze where

import Program
import Domain
import Worklist
import Data.Map (lookup, delete)

import LLVM.AST.Instruction as LLVMIR
import LLVM.AST.Type
import LLVM.AST.Global
import LLVM.AST.Operand as LLVMIR
import LLVM.AST.Float as LLVMIR
import LLVM.AST.Constant as Constant
import LLVM.AST.IntegerPredicate
import LLVM.AST.Name
import Data.Foldable (foldl')

import Debug.Trace
  
evalArgument :: Operand -> State -> Interval
evalArgument operand state =
  case operand of
    ConstantOperand const -> evalConst const
    LocalReference _ name -> findState (show name) state
    _                     -> undefined


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
evalConst _                          = error "non-Int/non-Float arg value not supported"


transferInstCall :: [CFG] -> State -> Named Instruction -> State
transferInstCall cfgPool state (name := LLVMIR.Call _ _ _ callee args _ _) =
  let args'       = map (\(x, y) -> x) args
      argIntvs    = map (\arg ->
                          case arg of
                            LocalReference _ name -> lookupWithExn (show name) state
                            ConstantOperand const -> evalConst const) args'
      calleeCFG   = findCFGByName callee cfgPool
      calleeTbl   = analyze argIntvs cfgPool calleeCFG 
      retBlock    = findRetBlock calleeCFG
      retState    = lookupWithExn retBlock calleeTbl
      retInstr    = getTerminator retBlock
      retIntv     = findRetInterval retInstr retState in
    bindState (show name) retIntv state
transferInstCall _ state (Do (LLVMIR.Call _ _ _ _ _ _ _)) = state


transferInstAlloca :: State -> Named Instruction -> State
transferInstAlloca state (name := LLVMIR.Alloca _ _ _ _) =
  bindState (show name) Bot state


transferInstLoad :: State -> Named Instruction -> State
transferInstLoad state (name := LLVMIR.Load _ address _ _ _) =
  case address of
    LocalReference _ name' -> let pointerItv = lookupWithExn (show name') state in
                               bindState (show name) pointerItv state
    ConstantOperand _     -> error $ "Constant load not supported: " ++ (show address)
    _                     -> undefined
  

transferInstStore :: State -> Named Instruction -> State
transferInstStore state (Do (LLVMIR.Store _ address value _ _ _)) =
  case value of
    LocalReference _ valName -> let valueItv = lookupWithExn (show valName) state in
                                  case address of
                                    LocalReference _ addressName ->
                                      -- perform a strong update
                                      let state' = delete (show addressName) state in
                                        bindState (show addressName) valueItv state'
                                    _ -> error $ "Store value not supported: " ++ (show value)
    ConstantOperand const    -> let valueItv = evalConst const in
                                  case address of
                                    LocalReference _ addressName ->
                                      -- perform a strong update
                                      let state' = delete (show addressName) state in
                                        bindState (show addressName) valueItv state'
                                    _ -> error $ "Store value not supported: " ++ (show value)
    _                        -> undefined


-- | Does the block end with Ret?
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


findRetInterval :: Terminator -> State -> Interval
findRetInterval (Ret (Just operand) _) state =
  case operand of
    LocalReference _ name ->
      lookupWithExn (show name) state
    ConstantOperand const -> evalConst const
    _                     -> undefined
findRetOperand (Ret Nothing _) = error "There are no return values; this is impossible."


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
    ConstantOperand (GlobalReference _ name) ->
      let matches = foldl (\acc cfg ->
                             if fid cfg == show name then cfg:acc else acc) [] cfgPool in
        case matches of
          []     -> error "could not find such CFG"
          [cfg]  -> cfg
          (x:xs) -> error "multiple CFGs exist with that name"
    MetadataOperand _ -> error "Metadata callable operand not supported"


transferInst :: [CFG] -> State -> Named Instruction -> State
transferInst cfgPool state instr = case instr of
  _ := LLVMIR.Add _ _ _ _ _      -> transferInstAdd state instr
  _ := LLVMIR.Sub _ _ _ _ _      -> transferInstSub state instr
  _ := LLVMIR.Mul _ _ _ _ _      -> transferInstMul state instr
  _ := LLVMIR.ICmp _ _ _ _       -> transferInstICmp state instr
  _ := LLVMIR.Phi _ _ _          -> transferInstPhi state instr
  _ := LLVMIR.Call _ _ _ _ _ _ _ -> transferInstCall cfgPool state instr
  _ := LLVMIR.Alloca _ _ _ _     -> transferInstAlloca state instr
  _ := LLVMIR.Load _ _ _ _ _     -> transferInstLoad state instr
  Do (LLVMIR.Store _ _ _ _ _ _)  -> transferInstStore state instr
  _ -> state


-- | Transfer a whole block.
transferBlock :: [CFG] -> State -> [Named Instruction] -> State
transferBlock cfgPool state instrs = foldl' (\acc instr -> transferInst cfgPool acc instr) state instrs


getParamName :: Parameter -> String
getParamName (Parameter _ name _) = show name

  
inputOf :: Node -> CFG -> Table -> [Interval] -> State
inputOf here cfg table argItvs =
  case isEntry cfg here of
    True  -> let beforeUpdate = foldl (\acc param ->
                                         bindState (getParamName param) Top acc) emptyState (getParams cfg)
                 paramNames   = map (\(Parameter _ name _) -> show name) (getParams cfg) in
               batchBindArgs argItvs paramNames beforeUpdate
    False -> let beforeUpdate = foldl (\acc param ->
                                         let res = findTable param table in
                                           stateJoin res acc) emptyState (predOfBlock cfg here)
                 paramNames   = map (\(Parameter _ name _) -> show name) (getParams cfg) in
               batchBindArgs argItvs paramNames beforeUpdate


needWiden :: Node -> Bool
needWiden _ = True


-- | The core worklist algorithm.
analyzeInner :: [Interval] -> [CFG] -> CFG -> Table -> Worklist -> Table
analyzeInner _ _ _ table [] = table
analyzeInner argItvs cfgPool cfg table wklist =
  let (here, wklist') = pop wklist
      state           = inputOf here cfg table argItvs
      state'          = transferBlock cfgPool state (getInstrs here)
      oldState        = findTable here table in
    if not $ stateOrder state' oldState
    then let table'   = if needWiden here
                        then bindTable here (stateWiden oldState state') table
                        else bindTable here (stateJoin oldState state') table
             wklist'' = addSet wklist' (succOfBlock cfg here) in
           (analyzeInner argItvs cfgPool cfg table' wklist'')
    else (analyzeInner argItvs cfgPool cfg table wklist')


analyze :: [Interval] -> [CFG] -> CFG -> Table
analyze argItvs cfgPool cfg =
  let wklist = addSet newWorklist (getBlocks cfg) in
    analyzeInner argItvs cfgPool cfg newTable wklist
