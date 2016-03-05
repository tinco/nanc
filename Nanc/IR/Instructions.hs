module Nanc.IR.Instructions where

import Debug.Trace

import Control.Monad.State

import LLVM.General.AST hiding (Module)

import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Constant as C


import qualified LLVM.General.AST.IntegerPredicate as I
import qualified LLVM.General.AST.FloatingPointPredicate as F

import Nanc.CodeGenState
import Nanc.IR.Types
import Nanc.IR.Expression.Helpers
import Nanc.AST

-- IEEE 754 double
double :: Type
double = FloatingPointType 64 IEEE

instr :: Type -> Instruction -> Codegen Operand
instr t ins = do
	n   <- fresh
	blk <- current
	let i = stack blk
	let ref = (UnName n)
	modifyBlock $ blk { stack = i ++ [ref := ins] }
	return $ local ref t

binInstr op a b = instr (IntegerType 1) $ op (fst a) (fst b) []

notInstr :: Type -> Operand -> Codegen Operand
notInstr t@(IntegerType bits) a = instr t $ Xor a (ConstantOperand $ C.Int bits 1) []

compInstr :: Type -> Operand -> Codegen Operand
compInstr t a = instr t $ Xor a wordMax []

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
	name <- getBlock
	blk <- current
	case term blk of
		Nothing -> modifyBlock $ blk { term = Just trm }
		Just term -> error ("Defining second terminator for block named '" ++ (show name) ++ "': " ++ (show blk))
	return trm

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

trunc :: Operand -> Integer -> Codegen Operand
trunc o i = instr t $ Trunc o t []
	where
		t = IntegerType (fromIntegral i)

add :: Type -> Operand -> Operand -> Codegen Operand
add t a b = instr t $ Add False False a b []

sub :: Type -> Operand -> Operand -> Codegen Operand
sub t a b = instr t $ Sub False False a b []

mul :: Type -> Operand -> Operand -> Codegen Operand
mul t a b = instr t $ Mul False False a b []

lor :: Type -> Operand -> Operand -> Codegen Operand
lor t a b = instr t $ Or a b []

land :: Type -> Operand -> Operand -> Codegen Operand
land t a b = instr t $ And a b []

xor :: Type -> Operand -> Operand -> Codegen Operand
xor t a b = instr t $ Xor a b []

sdiv :: Type -> Operand -> Operand -> Codegen Operand
sdiv t a b = instr t $ SDiv True a b []

srem :: Type -> Operand -> Operand -> Codegen Operand
srem t a b = instr t $ SRem a b []

shl :: Type -> Operand -> Operand -> Codegen Operand
shl t a b = instr t $ Shl False False a b []

shr :: Type -> Operand -> Operand -> Codegen Operand
shr t a b = instr t $ LShr False a b []

icmp :: I.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp p a b = instr booleanType $ ICmp p a b []

fcmp :: F.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp p a b = instr booleanType $ FCmp p a b []

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr double $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr double $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr double $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr double $ FDiv NoFastMathFlags a b []

br :: Name -> Codegen ()
br val = do
	currentBlockName <- getBlock
	bs <- current
	if hasTerminator bs
		then traceM ("Unreachable code in block: " ++ (show currentBlockName))
		else void $ terminator $ Do $ Br val []

brIfNoTerm :: Name -> Codegen ()
brIfNoTerm n = do
	bs <- current
	if hasTerminator bs
		then return ()
		else void $ br n

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Maybe Operand -> Codegen ()
ret val = do
	currentBlockName <- getBlock
	bs <- current
	if hasTerminator bs
		then traceM ("Unreachable return in block: " ++ (show currentBlockName))
		else void $ terminator $ Do $ Ret val []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr double $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr (PointerType ty (AddrSpace 0)) $ Alloca ty Nothing 0 []

store :: Type -> Operand -> Operand -> Codegen Operand
store t ptr val = instr t $ Store False ptr val Nothing 0 []
--	where
--		ptr' = trace ("Going to store ptr is: " ++  show ptr) ptr
--		val' = trace ("Going to store val is: " ++  show val) val

load :: Type -> Operand -> Codegen Operand
load t ptr = instr t $ Load False ptr Nothing 0 []

declare :: TypeTable -> Declaration -> Codegen ()
-- TODO we dont do anything yet with declarations
-- when we get a typechecker we should obviously
declare ts decl = do
		addr <- alloca t
		assign name (addr, typ)
	where
		name = declarationName decl
		typ = declarationType decl
		t = qualifiedTypeToType ts typ

operandToType :: Operand -> Type
operandToType (LocalReference t _) = t
operandToType (ConstantOperand c) = contantOperandToType c
operandToType _ = MetadataType

contantOperandToType :: C.Constant -> Type
contantOperandToType (C.GlobalReference t _) = t
contantOperandToType c = trace ("Unknown constant type: " ++ show c) undefined
