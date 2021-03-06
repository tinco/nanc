module Nanc.IR.Statement where

import Debug.Trace
import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.State

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Expression
import Nanc.IR.Expression.Binary
import Nanc.IR.Expression.Helpers
import Nanc.IR.Instructions

generateStatement :: TypeTable -> CStat -> Codegen ()
generateStatement ts (CExpr (Just expr) _) = void $ expressionValue ts expr
generateStatement ts (CExpr Nothing _) = return ()
generateStatement _ (CReturn Nothing _)= void $ ret Nothing
generateStatement ts (CReturn (Just expr) _) = void $ expressionValue ts expr >>= ret . Just . fst
generateStatement ts (CCompound _ident items _) = mapM_ (generateBlockItem ts) items
generateStatement ts (CIf expr trueStat maybeElseStat _) = generateIfStatement ts expr trueStat maybeElseStat
generateStatement ts (CFor (Left (maybeExpr1)) maybeExpr2 maybeExpr3 stat _) = generateForStatement ts maybeExpr1 maybeExpr2 maybeExpr3 stat
--generateStatement ts (CFor (Right decl) (Maybe (CExpression a)) (Maybe (CExpression a)) (CStatement a) a
generateStatement ts (CWhile maybeExpr stat isDoWhile _) = generateWhileStatement ts maybeExpr isDoWhile stat
generateStatement ts (CCont _) = void $ generateContinue
generateStatement ts (CBreak _) = void $ generateBreak
generateStatement ts (CLabel (Ident n _ _) stat [] _) = generateLabel ts n stat
generateStatement ts (CGoto (Ident n _ _) _) = void $ generateGoto ts n
generateStatement ts (CSwitch expr stat _) = generateSwitch ts expr stat
generateStatement ts (CCase const stat _) = generateCase ts const stat
generateStatement ts (CDefault stat _) = generateDefault ts stat
generateStatement _ _d = trace ("Unknown generateStatement: " ++ show _d) $ undefined

-- Make labels have their own namespace
labelName :: String -> String
labelName n = n ++ ":label:"

zeroReturn :: Codegen ()
zeroReturn = void $ ret $ Just (intConst 0)

generateLabel :: TypeTable -> String -> CStat -> Codegen ()
generateLabel ts name statement = do
	-- TODO throw error if label already exists
	labelBlock <- addBlock $ labelName name
	br labelBlock

	setBlock labelBlock
	generateStatement ts statement

generateGoto :: TypeTable -> String -> Codegen ()
generateGoto ts name = do
	labelBlock <- findBlock $ labelName name
	br labelBlock

generateBlockItem :: TypeTable -> CBlockItem -> Codegen ()
generateBlockItem ts (CBlockStmt stat) = generateStatement ts stat
generateBlockItem ts (CBlockDecl decl) = mapM_ (declare ts) declarations
	where
		declarations = buildDeclarations decl
generateBlockItem _ i = trace ("unknown generate block item: " ++ (show i) ) $ undefined

generateContinue :: Codegen ()
generateContinue = do
	entry <- currentLoopEntry
	br entry

generateBreak :: Codegen ()
generateBreak = do
	exit <- currentLoopExit
	br exit

generateSwitch :: TypeTable -> CExpr -> CStat -> Codegen ()
generateSwitch ts expr stat = do
	switchEntry <- addBlock "switch.entry"
	switchExit <- addBlock "switch.exit"
	switchDefault <- addBlock "switch.default"

	---- A continue inside a switch actually refers to
	---- any enclosing loop. Hopefully this works
	-----------------------
	switchContinue <- currentLoopEntry

	pushLoop switchContinue switchExit
	pushSwitch $ SwitchContext [] switchDefault

	-- The switch statement contains a bunch of statements
	-- most of them case statements. After they are all defined
	-- we jump to the switchEntry
	generateStatement ts stat
	brIfNoTerm switchEntry

	switchValue <- expressionValue ts expr

	-- now switch context should have all entries for cases
	let
		makeCase :: (AST.Name, AST.Name) -> (CExpr, AST.Name, AST.Name) -> Codegen (AST.Name, AST.Name)
		makeCase (nextEntry, nextBody) (const, body, exit) = do
			entryBlock <- addBlock "switch.entry.case"
			setBlock entryBlock
			constValue <- expressionValue ts const
			(op, _) <- binaryOp ts CEqOp switchValue constValue
			cbr op body nextEntry

			setBlock exit
			brIfNoTerm nextBody

			return (entryBlock, body)

	context <- popSwitch

	(firstCaseEntry, _) <- foldM makeCase (switchDefault, switchDefault) $ reverse $ cases context

	---- Enter into switch
	setBlock switchEntry
	br firstCaseEntry

	-- After default we break to exit of switch
	setBlock switchDefault
	brIfNoTerm switchExit

	---- Switch exit
	-----------------
	setBlock switchExit
	void popLoop

generateCase :: TypeTable -> CExpr -> CStat -> Codegen ()
generateCase ts const stat = do
	-- Store the current block since we're going to emit a
	-- new block that will not actually be linked to from the
	-- current block.
	prevBlock <- getBlock

	body <- addBlock "switch.body.case"
	setBlock body
	generateStatement ts stat

	exitBlock <- getBlock

	addSwitchCase const body exitBlock

	-- The terminator of the switch.body.case is set in
	-- the generateSwitchStatement function, not here.

	void $ setBlock prevBlock

generateDefault :: TypeTable -> CStat -> Codegen ()
generateDefault ts stat = do
	-- Store the current block since we're going to emit a
	-- new block that will not actually be linked to from the
	-- current block.
	prevBlock <- getBlock
	body <- getSwitchDefault

	setBlock body
	generateStatement ts stat
	-- The terminator of the switchDefault is set in
	-- the generateSwitchStatement function, not here.

	void $ setBlock prevBlock

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

	pushLoop condBlock exitBlock

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

	brIfNoTerm condBlock

	bodyBlock <- getBlock

	----
	------------------
	setBlock exitBlock
	popLoop

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
		then do
			pushLoop bodyBlock exitBlock
			br bodyBlock
		else do
			pushLoop condBlock exitBlock
			br condBlock

	---- Condition
	----------------
	setBlock condBlock
	cond <- boolOrCast ts expr
	cbr cond bodyBlock exitBlock

	---- Body
	-----------------
	setBlock bodyBlock
	generateStatement ts stat
	brIfNoTerm condBlock

	---- Exit
	-----------------
	setBlock exitBlock
	popLoop

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
	brIfNoTerm ifexit
	ifthen <- getBlock

	-- if.else
	------------------
	case maybeElseStat of
		Just elseState -> do
			setBlock ifelse
			generateStatement ts elseState
			brIfNoTerm ifexit
			ifelse <- getBlock
			return ()
		Nothing -> do
			setBlock ifelse
			brIfNoTerm ifexit
			ifelse <- getBlock
			return ()

	-- if.exit
	------------------
	setBlock ifexit
	-- will this work? do we need this at all?
	return ()