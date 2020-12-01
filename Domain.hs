module Domain where

{- Sign domain and its operations. -}

import Program

import qualified Data.Map as Map
import Numeric.IEEE (nan, infinity)
import qualified LLVM.AST.Global as LLVMIR
import qualified LLVM.AST.Operand as LLVMIR

data Interval
  = Top
  | Interval Float Float
  | Bot
  deriving (Eq)


instance Show Interval where
  show Top = "Top"
  show (Interval v1 v2) = "["++show v1++", "++show v2++"]"
  show Bot = "Bot"


getHigh :: Interval -> Float
getHigh (Interval v1 _) = v1
getHigh _ = nan


getLow :: Interval -> Float
getLow (Interval _ v2) = v2
getLow _ = nan


interFromInt :: Int -> Interval
interFromInt concreteInt = Interval (fromIntegral concreteInt) (fromIntegral concreteInt)


interFromInteger :: Integer -> Interval
interFromInteger concreteInt = Interval (fromIntegral concreteInt) (fromIntegral concreteInt)


interFromFloat :: Float -> Interval
interFromFloat concreteFloat = Interval concreteFloat concreteFloat


interPlus :: Interval -> Interval -> Interval
interPlus Bot _ = Bot
interPlus Top i2 =
  case i2 of
    Bot -> Bot
    _   -> Top
interPlus (Interval v1 v2) i2 =
  case i2 of
    Bot -> Bot
    Top -> Top
    Interval v3 v4 ->
      let low_sum  = v1 + v3
          high_sum = v2 + v4 in
        if low_sum < high_sum
        then Interval low_sum high_sum
        else Interval high_sum low_sum


interMinus :: Interval -> Interval -> Interval
interMinus Bot _ = Bot
interMinus Top i2 =
  case i2 of
    Bot -> Bot
    _   -> Top
interMinus (Interval v1 v2) i2 =
  case i2 of 
    Bot -> Bot
    Top -> Top
    Interval v3 v4 ->
      let low_subtract  = v1 - v3
          high_subtract = v2 - v4 in
        if low_subtract < high_subtract
        then Interval low_subtract high_subtract
        else Interval high_subtract low_subtract


interMult :: Interval -> Interval -> Interval
interMult Bot _ = Bot
interMult Top i2 =
  case i2 of
    Bot -> Bot
    _   -> Top
interMult (Interval v1 v2) i2 =
  case i2 of
    Bot -> Bot
    Top -> Top
    Interval v3 v4 ->
      let low_multiply  = v1 * v3
          high_multiply = v2 * v4 in
        if low_multiply < high_multiply
        then Interval low_multiply high_multiply
        else Interval high_multiply low_multiply


interSLE :: Interval -> Interval -> Interval
interSLE Bot _ = Bot
interSLE Top _ = Top
interSLE (Interval v1 v2) i2 =
  case i2 of
    Bot -> Bot
    Top -> Top
    Interval v3 v4 ->
      if v2 <= v3 then Interval 1.0 1.0 else
        if v4 < v1 then Interval 0.0 0.0 else Top


interSGE :: Interval -> Interval -> Interval
interSGE Bot _ = Bot
interSGE Top _ = Top
interSGE (Interval v1 v2) i2 =
  case i2 of
    Bot -> Bot
    Top -> Top
    Interval v3 v4 ->
      if v2 <= v3 then Interval 0.0 0.0 else
        if v4 < v1 then Interval 1.0 1.0 else Top

  
interOrder :: Interval -> Interval -> Bool
interOrder i1 i2
  | i1 == i2 = True
  | otherwise =
      case (i1, i2) of
        (_, Top) -> True
        (_, Bot) -> i1 == Bot
        (Top, i2) -> i2 == Top
        (Bot, _) -> True
        (Interval v1 v2, Interval v3 v4) -> v3 <= v1 && v2 <= v4


interJoin :: Interval -> Interval -> Interval
interJoin i1 i2 =
  case (i1, i2) of
    (Top, _) -> Top
    (_, Top) -> Top
    (Bot, i2) -> i2
    (i1, Bot) -> i1
    (Interval v1 v2, Interval v3 v4) -> if interOrder i1 i2 then i2 else
                                          if interOrder i2 i1 then i1 else
                                            if v3 <= v1 && v4 <= v2 then
                                              Interval v3 v2 else
                                              if v1 < v3 && v2 < v4 then
                                                Interval v1 v4 else
                                                error "Unreachable"


interWiden :: Interval -> Interval -> Interval
interWiden Top _ = Top
interWiden _ Top = Top
interWiden Bot i2 =
  case i2 of
    Interval v1 v2 -> Interval v1 infinity
    Top -> Top
    Bot -> Bot
interWiden _ Bot = Bot
interWiden (Interval v1 v2) (Interval v3 v4)
  | v1 == v3 && v2 /= v4 = if v2 >= v4 then (Interval v1 v2) else (Interval v1 infinity)
  | v2 == v4 && v1 /= v3 = if v1 <= v3 then (Interval v1 v2) else (Interval (-infinity) v2)
  | v1 == v3 && v2 == v4 = Interval v1 v2
  | v2 /= v4 && v1 /= v3 = if interOrder (Interval v1 v2) (Interval v3 v4) then (Interval v3 v4) else
                             if interOrder (Interval v3 v4) (Interval v1 v2) then (Interval v1 v2) else
                               interJoin (Interval v1 v2) (Interval v3 v4)


type State = Map.Map String Interval


emptyState :: State
emptyState = Map.empty


bindState :: String -> Interval -> State -> State
bindState = Map.insert


findState :: String -> State -> Interval
findState var abstractState =
  case Map.lookup var abstractState of
    Just interval -> interval
    Nothing   -> Bot


-- | True iff s1[var] <= s2[var] for all var.
stateOrder :: State -> State -> Bool
stateOrder s1 s2 = Map.foldlWithKey (\acc var inter1 ->
                                       interOrder inter1 (findState var s2) && acc) True s1


-- | Join two states pointwise. The default value is Bot.
stateJoin :: State -> State -> State
stateJoin s1 s2 =
  let newState = Map.empty
      state'   = Map.foldlWithKey (\acc k v ->
                                     bindState k v acc) newState s2
      state''  = Map.foldlWithKey (\acc k v1 ->
                                     bindState k (interJoin v1 (findState k acc)) acc) state' s1 in
    state''


-- | statewise widening. 
stateWiden :: State -> State -> State
stateWiden s1 s2 =
  let newState = Map.empty
      state'   = Map.foldlWithKey (\acc k v ->
                                     bindState k v acc) newState s2
      state''  = Map.foldlWithKey (\acc k v ->
                                     bindState k (interWiden v (findState k acc)) acc) state' s1 in
    state''


-- | Pretty print a state.
stateToString :: State -> String
stateToString s
  | s == Map.empty = "{ }"
  | otherwise  = Map.foldlWithKey (\acc var sign ->
                                     acc ++ "\t" ++ var ++ " |-> " ++ (show sign) ++ "\n") "" s


type Table = Map.Map Node State


newTable :: Table
newTable = Map.empty


bindTable :: Node -> State -> Table -> Table
bindTable = Map.insert


batchBindArgs :: [Interval] -> [String] -> State -> State
batchBindArgs argItvs paramNames state =
  let paramAndItvs = zip paramNames argItvs in
    foldl (\acc (paramName, itv) -> bindState paramName itv acc) state paramAndItvs


findTable :: Node -> Table -> State
findTable node table =
  case Map.lookup node table of
    Just state -> state
    Nothing    -> emptyState


tableToString :: Table -> String
tableToString table = Map.foldlWithKey (\acc node state ->
                                          acc ++ "   " ++ (show $ getName node) ++ "\n"
                                         ++ (stateToString state) ++ "\n") "" table
