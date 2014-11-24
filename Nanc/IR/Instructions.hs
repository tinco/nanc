module Nanc.IR.Instructions where

import Control.Monad.State

import LLVM.General.AST hiding (Module)

import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC

import Nanc.CodeGenState

-- IEEE 754 double
double :: Type
double = FloatingPointType 64 IEEE

getvar :: String -> Codegen Operand
getvar var = do
	syms <- gets symtab
	case lookup var syms of
		Just x  -> return x
		Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Type -> Instruction -> Codegen Operand
instr t ins = do
	n   <- fresh
	blk <- current
	let i = stack blk
	let ref = (UnName n)
	modifyBlock $ blk { stack = i ++ [ref := ins] }
	return $ local ref t

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
	blk <- current
	modifyBlock $ blk { term = Just trm }
	return trm

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr double $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr double $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr double $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr double $ FDiv NoFastMathFlags a b []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr double $ Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr double $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr double $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr double $ Load False ptr Nothing 0 []