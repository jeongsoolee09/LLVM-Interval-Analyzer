module Program where

import Data.Map
import qualified Data.Map.Internal.Debug as Debug
import Data.Maybe
import qualified LLVM.AST.Instruction as LLVMIR
import qualified LLVM.AST.Type as LLVMIR
import qualified LLVM.AST.Global as LLVMIR
import qualified LLVM.AST.Name as LLVMIR
import qualified LLVM.AST as A
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.ByteString.Short as ByteString (fromShort)
import qualified Data.ByteString.UTF8 as ByteString (toString)


type Node = LLVMIR.BasicBlock


-- Dummy instance declaration to use BasicBlock in a map.
instance Ord LLVMIR.BasicBlock where
  compare n1 n2 = Prelude.compare (getName n1) (getName n2)
  (<=) n1 n2 = (<=) (getName n1) (getName n2)


data MyModule = MyModule {typeDefs :: [LLVMIR.Type],
                          globals :: [LLVMIR.Global],
                          cfgs :: [CFG]}


data CFG = CFG {fid :: String,
                sig :: LLVMIR.Type, -- this should always be a FunctionType
                params :: [LLVMIR.Parameter],
                blocks :: [Node],
                succ :: Map Node [Node],
                pred :: Map Node [Node]}


filterTypeDefs :: [A.Definition] -> [LLVMIR.Type]
filterTypeDefs lst = catMaybes $ filterTypeDefsInner lst []
  where
    filterTypeDefsInner [] acc = acc
    filterTypeDefsInner (x:xs) acc = case x of
                                       A.TypeDefinition _ maybeType -> filterTypeDefsInner xs (maybeType:acc)
                                       x -> filterTypeDefsInner xs acc


getTypeDefs :: A.Module -> [LLVMIR.Type]
getTypeDefs (A.Module _ _ _ _ moduleDefs) = filterTypeDefs moduleDefs


filterGlobals :: [A.Definition] -> [LLVMIR.Global]
filterGlobals lst = filterGlobalsInner lst []
  where
    filterGlobalsInner [] acc = acc
    filterGlobalsInner (x:xs) acc = case x of
                                      A.GlobalDefinition globaldef -> filterGlobalsInner xs (globaldef:acc)
                                      x -> filterGlobalsInner xs acc

                                      
getGlobals :: A.Module -> [LLVMIR.Global]
getGlobals (A.Module _ _ _ _ moduleDefs) = filterGlobals moduleDefs


filterFuncsInner :: [LLVMIR.Global] -> [LLVMIR.Global] -> [LLVMIR.Global]
filterFuncsInner [] acc = acc
filterFuncsInner (x:xs) acc = case x of
  A.Function _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> filterFuncsInner xs (x:acc)
  x -> filterFuncsInner xs acc


filterFuncs :: [LLVMIR.Global] -> [LLVMIR.Global]
filterFuncs lst = filterFuncsInner lst []


getFuncDefs :: [LLVMIR.Global] -> [LLVMIR.Global]
getFuncDefs globalDefs = filterFuncs globalDefs


newModule :: A.Module -> MyModule
newModule amodule =
  let typeDefs = getTypeDefs amodule
      globals  = getGlobals amodule
      funcDefs = getFuncDefs globals
      cfgs     = Prelude.map newCfg funcDefs in
  MyModule typeDefs globals cfgs


findBasicBlockByName :: [Node] -> A.Name -> Node
findBasicBlockByName [] _ = error "could not find such block!"
findBasicBlockByName (block:blocks) targetName =
  if (getName block) == targetName then block else findBasicBlockByName blocks targetName

{- Accessors -}

-- Accessors for MyModules
getCFGs :: MyModule -> [CFG]
getCFGs (MyModule _ _ cfgs) = cfgs

-- Accessors for CFGs
  
getFid :: CFG -> String
getFid (CFG fid _ _ _ _ _) = fid


getParams :: CFG -> [LLVMIR.Parameter]
getParams (CFG _ _ params _ _ _) = params


getBlocks :: CFG -> [Node]
getBlocks (CFG _ _ _ blocks _ _) = blocks


getSuccs :: CFG -> Map Node [Node]
getSuccs (CFG _ _ _ _ succs _) = succs


getPreds :: CFG -> Map Node [Node]
getPreds (CFG _ _ _ _ _ preds) = preds

-- Accesors for Nodes

getName :: Node -> A.Name
getName (LLVMIR.BasicBlock name _ _) = name

getInstrs :: Node -> [A.Named A.Instruction]
getInstrs (LLVMIR.BasicBlock _ instrs _) = instrs
  
{- Accessors End -} 

succs :: [Node] -> LLVMIR.Terminator -> [Node]
succs blockPool (LLVMIR.Ret _ _) = []
succs blockPool (LLVMIR.Br dest _) = [findBasicBlockByName blockPool dest]
succs blockPool (LLVMIR.CondBr _ trueDest falseDest _) =
  let trueBlock = findBasicBlockByName blockPool trueDest
      falseBlock = findBasicBlockByName blockPool falseDest in
    [trueBlock, falseBlock]
succs blockPool (LLVMIR.Switch _ defaultDest dests _) =
  let defaultDestBlock = findBasicBlockByName blockPool defaultDest
      destNames        = Prelude.map (\(x, y) -> y) dests
      destBlocks       = Prelude.map (findBasicBlockByName blockPool) destNames in
    (defaultDestBlock : destBlocks)
succs blockPool (LLVMIR.IndirectBr _ destNames _) =
  Prelude.map (findBasicBlockByName blockPool) destNames
succs blockPool (LLVMIR.Invoke _ _ _ _ _ returnDestName exceptionDestName _) =
  let returnDestBlock    = findBasicBlockByName blockPool returnDestName
      exceptionDestBlock = findBasicBlockByName blockPool exceptionDestName in
    [returnDestBlock, exceptionDestBlock]
succs blockPool (LLVMIR.Resume _ _) = []
succs blockPool (LLVMIR.CatchSwitch _ catchHandlerNames defaultUnwindDestName _) =
  let catchHandlerNames_ = NonEmptyList.toList catchHandlerNames
      catchHandlerBlocks = Prelude.map (findBasicBlockByName blockPool) catchHandlerNames_ in
  case defaultUnwindDestName of
    Just destName ->
      let destBlock = findBasicBlockByName blockPool destName in
        (destBlock : catchHandlerBlocks)
    Nothing       -> catchHandlerBlocks
succs blockPool (LLVMIR.Unreachable block) = []
succs blockPool (LLVMIR.CatchRet _ succ _) = [findBasicBlockByName blockPool succ]
succs blockPool (LLVMIR.CleanupRet _ unwindDestName _) =
  case unwindDestName of
    Just destName -> [findBasicBlockByName blockPool destName]
    Nothing       -> []


getTerminator :: Node -> LLVMIR.Terminator
getTerminator (LLVMIR.BasicBlock _ _ (A.Do terminator)) = terminator
getTerminator (LLVMIR.BasicBlock _ _ (_ A.:= terminator)) = terminator
  

getSuccsOfBlock :: [Node] -> Node -> [Node]
getSuccsOfBlock blockPool targetBlock =
  let terminator = getTerminator targetBlock in
    succs blockPool terminator
  
  
type NodeRelation = Map Node [Node]


updateRelation relation key valList =
  case Data.Map.lookup key relation of
    Just valList' -> insert key (valList ++ valList') relation
    Nothing       -> insert key valList relation


getNodeRelations :: [Node] -> (NodeRelation, NodeRelation)
getNodeRelations blockPool =
  Prelude.foldl (\(succRel, predRel) block ->
           let succs    = getSuccsOfBlock blockPool block
               succRel' = updateRelation succRel block succs
               predRel' = Prelude.foldl (\acc succ ->
                                   case Data.Map.lookup succ predRel of
                                     Just _  -> updateRelation acc succ [block]
                                     Nothing -> updateRelation acc succ [block]) predRel succs in
           (succRel', predRel')) (Data.Map.empty, Data.Map.empty) blockPool 


newCfg :: LLVMIR.Global -> CFG
newCfg (LLVMIR.Function _ _ _ _ _ rtntype name params _ _ _ _ _ _ blocks _ _) =
  let fid  = name
      sig  = rtntype
      (succ, pred) = getNodeRelations blocks
      fst  = \(x,y) -> x in
  CFG (show fid) sig (fst params) blocks succ pred
newCfg _ = error "newCfg should be invoked only on function definitions"


isIn :: Eq b => b -> Map a [b] -> Bool
isIn val valListMap = foldlWithKey (\acc _ valList -> acc || elem val valList) False valListMap


type MaybeError a = Either String a


entryBlockInner :: CFG -> MaybeError Node
entryBlockInner cfg =
  let entryBlock = head $ getBlocks cfg
      predMap    = getPreds cfg
      succMap    = getSuccs cfg in
    case Data.Map.lookup entryBlock predMap of
      Just lst ->
        if length lst > 0 then Left "The entry block mismatch" else
          if entryBlock `isIn` succMap
            then Left "The first block is not the entry block"
            else Right entryBlock
      Nothing  -> Right entryBlock


entryBlock :: CFG -> Node
entryBlock cfg = case entryBlockInner cfg of
  Left errMsg -> error errMsg
  Right block -> block
  

isEntry :: CFG -> Node -> Bool
isEntry cfg node = node == entryBlock cfg


succOfBlock :: CFG -> Node -> [Node]
succOfBlock cfg node =
  let succMap = getSuccs cfg in
    case Data.Map.lookup node succMap of
      Just nodeList -> nodeList
      Nothing       -> []


predOfBlock :: CFG -> Node -> [Node]
predOfBlock cfg node =
  let predMap = getPreds cfg in
    case Data.Map.lookup node predMap of
      Just nodeList -> nodeList
      Nothing       -> []


lookupWithExn :: (Ord a, Show a) => a -> Map a b -> b
lookupWithExn key mapVal =
  case Data.Map.lookup key mapVal of
    Just sth -> sth
    Nothing  -> error $ "Lookup for " ++ show key ++ " has failed"


getBasicBlockName :: Node -> LLVMIR.Name
getBasicBlockName (LLVMIR.BasicBlock name _ _) = name


toGraphVizDotInner :: Map Node [Node] -> [Node] -> String -> String
toGraphVizDotInner succMap [] acc = acc
toGraphVizDotInner succMap (node:nodes) acc =
  let succBlocks = lookupWithExn node succMap
      acc' = acc ++ "\t" ++ (show $ getBasicBlockName node) ++ " -> " ++ show (Prelude.map getBasicBlockName succBlocks) ++ "\n" in
    toGraphVizDotInner succMap nodes acc'
  

toGraphVizDot :: CFG -> String
toGraphVizDot cfg =
  "digraph" ++ getFid cfg ++ "{\n" ++ toGraphVizDotInner (getSuccs cfg) (getBlocks cfg) "" ++ "}"


instance Show CFG where
  show = toGraphVizDot
  

debugMap :: NodeRelation -> IO ()
debugMap map =
    putStrLn $ Debug.showTreeWith (\k x -> show (getBasicBlockName k, Prelude.map getBasicBlockName x)) True True map
