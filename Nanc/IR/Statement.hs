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
generateStatement ts (CSwitch expr (CCompound [] caseStmnts _ ) _) = generateSwitch ts expr caseStmnts
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

generateSwitch :: TypeTable -> CExpr -> [CBlockItem] -> Codegen ()
generateSwitch ts expr switchStmnts = do
	switchEntry <- getBlock
	switchValue <- expressionValue ts expr

	switchExit <- addBlock "switch.exit"
	switchContinue <- addBlock "switch.continue"
	switchDefault <- addBlock "switch.default"

	pushLoop switchContinue switchExit

	defaultCase <- case defaults of
		[] -> return (switchExit, switchExit)
		[stat] -> do
			setBlock switchDefault
			generateStatement ts stat
			brIfNoTerm switchExit
			return (switchDefault, switchDefault)

	let
		makeCase :: (AST.Name, AST.Name) -> (CExpr, CStat) -> Codegen (AST.Name, AST.Name)
		makeCase (nextEntry, nextBody) (const, stat) = do
			entryBlock <- addBlock "switch.entry.case"
			bodyBlock <- addBlock "switch.body.case"

			setBlock entryBlock
			constValue <- expressionValue ts const
			(op, _) <- binaryOp ts CEqOp switchValue constValue
			cbr op bodyBlock nextEntry

			setBlock bodyBlock
			generateStatement ts stat
			brIfNoTerm nextBody

			return (entryBlock, bodyBlock)

	(firstCaseEntry, _) <- foldM makeCase defaultCase (reverse cases)

	---- Enter into switch
	setBlock switchEntry
	mapM_ (generateBlockItem ts) otherStatements
	br firstCaseEntry

	---- A continue inside a switch actually refers to
	---- any enclosing loop.
	-----------------------
	setBlock switchContinue
	popLoop
	loopStack <- gets loopEntryStack
	if loopStack == []
		then error "Not allowed to continue outside loop"
		else generateContinue

	---- Switch exit
	-----------------
	setBlock switchExit
	popLoop
	return ()

	where
		cases = map extractCase $ (filter isCase switchStmnts) ++ (filter isLabeledCase switchStmnts)
		extractCase (CBlockStmt (CCase const stat _)) = (const, stat)
		-- Rewrite labeled cases
		extractCase (CBlockStmt (CLabel i (CCase const caseStat _) [] n)) = (const, (CLabel i caseStat [] n))
		isCase (CBlockStmt (CCase _ _ _)) = True
		isCase _ = False
		isLabeledCase (CBlockStmt (CLabel _ (CCase _ _ _) _ _)) = True
		isLabeledCase _ = False
		defaults = map extractDefault $ filter isDefault switchStmnts
		isDefault (CBlockStmt (CDefault _ _)) = True
		isDefault _ = False
		extractDefault (CBlockStmt (CDefault stat _)) = stat
		otherStatements = [ stat | stat <- switchStmnts, not $ isCase stat, not $ isDefault stat, not $ isLabeledCase stat]

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