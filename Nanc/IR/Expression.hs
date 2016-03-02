module Nanc.IR.Expression where

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Char

import Control.Monad

import Language.C
import Language.C.Data.Ident
import Language.C.Pretty
import Language.C.Syntax.AST

import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as LT
import qualified LLVM.General.AST.IntegerPredicate as I
import qualified LLVM.General.AST.FloatingPointPredicate as F

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Instructions
import Nanc.IR.Expression.Binary
import Nanc.IR.Expression.Helpers

import qualified LLVM.General.AST.Constant as C

{-}
---
We have two main functions for expressions: expressionValue and expressionAddress

Given the following C snippet:

var x;
x += 1;
print(x);

We execute the following instructions:

declare x
v <- expressionValue 'x'
a <- expressionAddress 'x'
store a (v + 1)
v' <- load a
call 'print' [v']

---
 -}

type ExpressionResult = ( AST.Operand, QualifiedType )

expressionAddress :: TypeTable -> CExpr -> Codegen ExpressionResult

-- var
expressionAddress ts (CVar (Ident name _ _) _) = do
	(address, typ) <- getvar name
	let t = qualifiedTypeToType ts typ
	return (address, typ)

-- CVar (Ident "_p" 14431 n) n
expressionAddress ts (CMember subjectExpr (Ident memName _ _) _bool _) = do
	(addr, typ) <- expressionAddress ts subjectExpr
	let (i, resultType) = lookupMember ts typ memName

	let t = qualifiedTypeToType ts resultType
	let pt = LT.ptr t
	let idx = gepIndex $ fromIntegral i

	resultAddr <- instr pt (AST.GetElementPtr True addr [idx] [])
	return (resultAddr, resultType)

expressionAddress ts (CCast decl expr _) = do
		(addr, _) <- expressionAddress ts expr
		return (addr, t)
	where
		t = declarationType.buildDeclaration $ decl

expressionAddressys ts (CIndex subjectExpr expr _) = do
	(addr, typ) <- expressionAddress ts subjectExpr
	(index, _) <- expressionValue ts expr
	let t = qualifiedTypeToType ts typ
	delta <- mul (AST.IntegerType 64) (intConst64 $ fromIntegral $ sizeof t) index
	newAddr <- add (AST.IntegerType 64) addr delta
	return (newAddr, typ)

expressionValue :: TypeTable -> CExpr -> Codegen ExpressionResult

-- var()
expressionValue ts (CCall fn' args' _) = do
	args <- mapM (expressionValue ts) args'
	(fn,t) <- expressionValue ts fn'
	result <- call fn (map fst args)
	return (result, returnType t)

-- var
expressionValue ts (CVar (Ident name _ _) _) = do
	(address, typ) <- getvar name
	case typ of
		QualifiedType (FT _) _ -> return (address, typ)
		_ -> do
			let t = qualifiedTypeToType ts typ
			value <- load t address
			return (value, typ)

-- var = bar
expressionValue ts (CAssign CAssignOp leftExpr rightExpr _) = do
	(addr, typ) <- expressionAddress ts leftExpr
	(val, typ2) <- expressionValue ts rightExpr

	if typ == typ2
		then do 
			let t = qualifiedTypeToType ts typ2
			store t addr val
			return (val, typ)
		else error ("Assignment types aren't equal: " ++ (show typ) ++ " vs. " ++ (show typ2))

-- var *= bar
expressionValue ts (CAssign assignOp leftExpr rightExpr _) = do
	(addr, typ) <- expressionAddress ts leftExpr
	(oldVal, typ) <- expressionValue ts leftExpr
	(modifier, typ2) <- expressionValue ts rightExpr

	if typ == typ2
		then do
			let t = qualifiedTypeToType ts typ2
			val <- doOp assignOp t oldVal modifier
			store t addr val
			return (val, typ)
		else error ("Assignment types aren't equal: " ++ (show typ) ++ " vs. " ++ (show typ2))
	where
		doOp CMulAssOp = mul
		doOp CAddAssOp = add
		doOp CSubAssOp = sub
		doOp COrAssOp = lor
		doOp CAndAssOp = land
		doOp op = trace ("Unkown assignment operation: " ++ show op) undefined

-- *var
expressionValue ts (CUnary CIndOp expr _) = do
	traceM ("Dereferencing expression: " ++ (show expr)) 
	expressionAddress ts expr

-- var++
expressionValue ts (CUnary CPostIncOp expr _) = do
	(addr, typ) <- expressionAddress ts expr
	(val, typ) <- expressionValue ts expr

	let t = qualifiedTypeToType ts typ
	inc_val <- add t val (intConst 1)

	store t addr inc_val
	return (val, typ)

-- --var;
expressionValue ts (CUnary CPreDecOp expr _) = do
	(addr, typ) <- expressionAddress ts expr
	(val, typ) <- expressionValue ts expr

	let t = qualifiedTypeToType ts typ
	dec_val <- add t val (intConst (-1))

	store t addr dec_val
	return (dec_val, typ)

-- !var;
expressionValue ts (CUnary CNegOp expr _) = do
	(val, typ) <- expressionValue ts expr
	let t = qualifiedTypeToType ts typ
	result <- notInstr t val
	return (result, typ)

-- ~x; (bitwise NOT)
expressionValue ts (CUnary CCompOp expr _) = do
	(val, typ) <- expressionValue ts expr
	let t = qualifiedTypeToType ts typ
	result <- compInstr t val
	return (result, typ)

-- -x;
expressionValue ts (CUnary CMinOp expr _) = do
	(val, typ) <- expressionValue ts expr
	let t = qualifiedTypeToType ts typ
	result <- if isFloatType typ
		then fsub (floatConst 0.0) val
		else sub t (intConst 0) val

	return (result, typ)

--  Binary expressions
expressionValue ts (CBinary op leftExpr rightExpr _) = do
	(leftVal, typ) <- expressionValue ts leftExpr
	(rightVal, typ2) <- expressionValue ts rightExpr
	(result, t) <- binaryOp ts op (leftVal, typ) (rightVal, typ2)
	return (result, t)

-- CVar (Ident "_p" 14431 n) n
expressionValue ts m@(CMember subjectExpr (Ident memName _ _) _bool _) = do
	(addr, typ) <- expressionAddress ts m
	let t = qualifiedTypeToType ts typ
	value <- load t addr
	return (value, typ)

-- (CConst (CCharConst '\n' ()))
-- (CConst (CIntConst 0 ())) ())
expressionValue _ (CConst (CIntConst (CInteger i _ _) _)) = return (result, typ)
	where
		result = intConst64 $ fromIntegral i
		typ = QualifiedType (ST SignedInt) (defaultTypeQualifiers { typeIsConst = True })

expressionValue ts (CConst (CStrConst (CString str _) _)) = do
		name <- literal (cnst, typ)
		let t = qualifiedTypeToType ts typ
		let addr = global (AST.Name name) t
		result <- instr (AST.PointerType (AST.IntegerType 8) (AddrSpace 0)) (AST.GetElementPtr True addr [intConst64 0, intConst64 0] [])
		return (result, typ)
	where
		cnst = C.Array (AST.IntegerType 8) (map ((C.Int 8).fromIntegral.ord) str)
		typ = QualifiedType (Arr (fromIntegral $ length str) (QualifiedType (ST Char) constTypeQualifiers)) constTypeQualifiers

expressionValue ts (CConst (CCharConst (CChar chr _) _)) = return (result, typ)
	where
		result = intConst8 $ fromIntegral $ ord chr
		typ = QualifiedType (ST Char) (defaultTypeQualifiers { typeIsConst = True })
		
expressionValue _ (CConst c) = trace ("\n\nI don't know how to do CConst: " ++ (show c) ++ "\n\n") undefined
expressionValue ts (CCast decl expr _) = do
		(value, _) <- expressionValue ts expr
		return (value, t)
	where
		t = declarationType.buildDeclaration $ decl

expressionValue ts i@(CIndex subjectExpr expr _) = do
	(addr, typ) <- expressionAddress ts i
	let t = qualifiedTypeToType ts typ
	value <- load t addr
	return (value, typ)

expressionValue _ expr = trace ("IR Expression unknown node: " ++ (show expr)) undefined