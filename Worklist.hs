module Worklist where

import Program

import LLVM.AST.Instruction
import LLVM.AST.Type
import LLVM.AST.Global
import LLVM.AST.Operand
import LLVM.AST.Constant

  
type Worklist = [Node]


isEmpty :: Worklist -> Bool
isEmpty wklist = length wklist == 0


add :: Worklist -> Node -> Worklist
add wklist node = wklist ++ [node]


addSet :: Worklist -> [Node] -> Worklist
addSet wklist nodes = foldl (\acc node -> add acc node) wklist nodes


popInner :: Worklist -> Maybe (Node, Worklist) 
popInner wklist =
  if length wklist == 0
  then Nothing
  else let popped  = head wklist
           wklist' = tail wklist in
         Just (popped, wklist')


pop :: Worklist -> (Node, Worklist)
pop wklist = case popInner wklist of
  Just tuple -> tuple
  Nothing    -> error "Worklist is empty"


newWorklist :: Worklist
newWorklist = []
