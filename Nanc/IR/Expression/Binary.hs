module Nanc.IR.Expression.Binary where

import Debug.Trace

import Control.Monad

import Language.C

import Nanc.IR.Types
import Nanc.IR.Instructions
import Nanc.IR.Expression.Helpers

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.IntegerPredicate as I
import qualified LLVM.General.AST.FloatingPointPredicate as F

-- choose between icmp and fcmp
binaryOp :: TypeTable -> CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen (AST.Operand, QualifiedType)
binaryOp _ CLorOp a b = liftM2 (,) (binInstr AST.Or a b) (return $ snd a) 
binaryOp _ CLndOp a b = liftM2 (,) (binInstr AST.And a b) (return $ snd a)
binaryOp _ CNeqOp a b = liftM2 (,) (cmpOp CNeqOp a b) (return defaultBooleanType)
binaryOp _ CEqOp a b = liftM2 (,) (cmpOp CEqOp a b) (return defaultBooleanType)
binaryOp _ CGeqOp a b = liftM2 (,) (cmpOp CGeqOp a b) (return defaultBooleanType)
binaryOp _ CGrOp  a b = liftM2 (,) (cmpOp CGrOp a b) (return defaultBooleanType)
binaryOp ts CMulOp a b = liftM2 (,) (mul (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CSubOp a b = liftM2 (,) (sub (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CAddOp a b = liftM2 (,) (add (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CDivOp a b = liftM2 (,) (sdiv (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CRmdOp a b = liftM2 (,) (srem (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CShlOp a b = liftM2 (,) (shl (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CShrOp a b = liftM2 (,) (shr (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts COrOp a b = liftM2 (,) (lor (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CAndOp a b = liftM2 (,) (land (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)
binaryOp ts CXorOp a b = liftM2 (,) (xor (qualifiedTypeToType ts (snd a)) (fst a) (fst b)) (return $ snd a)


binaryOp _ op _ _ = trace ("Don't know how to binaryOp: " ++ (show op)) undefined

cmpOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen AST.Operand
cmpOp cmp a@(a',_) b | isInteger a' = iCmpOp cmp a b
                     | otherwise = fCmpOp cmp a b

iCmpOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen AST.Operand
iCmpOp cmp (a,t) (b,_) = icmp (iOpToPred (isSigned t) cmp) a b

fCmpOp :: CBinaryOp -> (AST.Operand, QualifiedType) -> (AST.Operand, QualifiedType) -> Codegen AST.Operand
fCmpOp cmp (a,t) (b,_) = fcmp (fOpToPred cmp) a b 

data FloatOrInt = Floaty | Intty
iOpToPred :: Bool -> CBinaryOp -> I.IntegerPredicate
iOpToPred _ CNeqOp = I.NE
iOpToPred True CGeqOp = I.SGE
iOpToPred False CGeqOp = I.UGE
iOpToPred True CGrOp = I.SGT
iOpToPred False CGrOp = I.UGT
iOpToPred _ CEqOp = I.EQ

fOpToPred :: CBinaryOp -> F.FloatingPointPredicate
fOpToPred CGeqOp = F.UGE
fOpToPred CNeqOp = F.UNE
fOpToPred CGrOp = F.UGT
fOpToPred CEqOp = F.UEQ