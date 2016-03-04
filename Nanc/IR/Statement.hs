module Nanc.IR.Statement where

import Debug.Trace
import Data.Maybe

import Control.Monad

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Expression
import Nanc.IR.Expression.Helpers
import Nanc.IR.Instructions

generateStatement :: TypeTable -> CStat -> Codegen ()
generateStatement ts (CExpr expr _) = void $ expressionValue ts (fromJust expr)
generateStatement _ (CReturn Nothing _)= void $ ret Nothing
generateStatement ts (CReturn (Just expr) _) = void $ expressionValue ts expr >>= ret . Just . fst
generateStatement ts (CCompound _ident items _) = mapM_ (generateBlockItem ts) items
generateStatement ts (CIf expr trueStat maybeElseStat _) = generateIfStatement ts expr trueStat maybeElseStat
generateStatement ts (CFor (Left (maybeExpr1)) maybeExpr2 maybeExpr3 stat _) = generateForStatement ts maybeExpr1 maybeExpr2 maybeExpr3 stat
--generateStatement ts (CFor (Right decl) (Maybe (CExpression a)) (Maybe (CExpression a)) (CStatement a) a
generateStatement ts (CWhile maybeExpr stat isDoWhile _) = generateWhileStatement ts maybeExpr isDoWhile stat
generateStatement _ _d = trace ("Unknown generateStatement: " ++ show _d) $ undefined

zeroReturn :: Codegen ()
zeroReturn = void $ ret $ Just (intConst 0)

generateBlockItem :: TypeTable -> CBlockItem -> Codegen ()
generateBlockItem ts (CBlockStmt stat) = generateStatement ts stat
generateBlockItem ts (CBlockDecl decl) = mapM_ (declare ts) declarations
	where
		declarations = buildDeclarations decl
generateBlockItem _ i = trace ("unknown generate block item: " ++ (show i) ) $ undefined

generateForStatement :: TypeTable -> Maybe CExpr -> Maybe CExpr -> Maybe CExpr -> CStat -> Codegen ()
generateForStatement ts maybeExpr1 maybeExpr2 maybeExpr3 stat = do
	-- %entry
	-------------
	case maybeExpr1 of 
		Just expr -> do 
			expressionValue ts expr
			return ()
		Nothing -> return ()

	condBlock <- addBlock "for.cond"
	bodyBlock <- addBlock "for.body"
	exitBlock <- addBlock "for.exit"

	br condBlock

	-----
	-----------------
	setBlock condBlock
	cond <- case maybeExpr2 of
		Just expr -> boolOrCast ts expr
		Nothing -> return $ boolConst 1

	cbr cond bodyBlock exitBlock
	condBlock <- getBlock

	----
	------------------
	setBlock bodyBlock
	generateStatement ts stat
	case maybeExpr3 of
		Just expr -> do 
			expressionValue ts expr
			return ()
		Nothing -> return ()

	br condBlock
	bodyBlock <- getBlock

	----
	------------------
	setBlock exitBlock
	return()

generateWhileStatement :: TypeTable -> CExpr -> Bool -> CStat -> Codegen ()
generateWhileStatement ts expr isDoWhile stat = do
	-- %entry
	-------------

	condBlock <- addBlock "while.cond"
	bodyBlock <- addBlock "while.body"
	exitBlock <- addBlock "while.exit"

	---- If it's a do-while we skip over condition the first run
	-----------------
	if isDoWhile
		then br bodyBlock
		else br condBlock

	---- Condition
	----------------
	setBlock condBlock
	cond <- boolOrCast ts expr
	cbr cond bodyBlock exitBlock

	---- Body
	-----------------
	setBlock bodyBlock
	generateStatement ts stat
	br condBlock

	---- Exit
	-----------------
	setBlock exitBlock
	return ()

generateIfStatement :: TypeTable -> CExpr -> CStat -> Maybe CStat -> Codegen ()
generateIfStatement ts condition trueStat maybeElseStat = do
	ifthen <- addBlock "if.then"
	ifelse <- addBlock "if.else"
	ifexit <- addBlock "if.exit"

	-- %entry
	------------------
	cond <- boolOrCast ts condition
	cbr cond ifthen ifelse

	-- if.then
	------------------
	setBlock ifthen
	generateStatement ts trueStat
	br ifexit
	ifthen <- getBlock

	-- if.else
	------------------
	case maybeElseStat of
		Just elseState -> do
			setBlock ifelse
			generateStatement ts elseState
			br ifexit
			ifelse <- getBlock
			return ()
		Nothing -> do
			setBlock ifelse
			br ifexit
			ifelse <- getBlock
			return ()

	-- if.exit
	------------------
	setBlock ifexit
	-- will this work? do we need this at all?
	return ()