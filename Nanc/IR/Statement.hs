module Nanc.IR.Statement where

import Debug.Trace
import Data.Maybe

import Control.Monad

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST

import Nanc.CodeGenState
import Nanc.AST

import Nanc.IR.Types
import Nanc.IR.Expression
import Nanc.IR.Instructions

generateStatement :: TypeTable -> CStat -> Codegen ()
generateStatement ts (CExpr expr _) = void $ generateExpression ts (fromJust expr)
generateStatement _ (CReturn Nothing _)= void $ ret Nothing
generateStatement ts (CReturn (Just expr) _) = void $ generateExpression ts expr >>= ret . Just . fst3
generateStatement ts (CCompound _ident items _) = mapM_ (generateBlockItem ts) items
generateStatement ts (CIf expr trueStat maybeElseStat _) = generateIfStatement ts expr trueStat maybeElseStat
generateStatement _ _d = trace ("Unknown generateStatement: " ++ show _d) $ undefined

zeroReturn :: Codegen ()
zeroReturn = void $ ret $ Just (intConst 0)

generateBlockItem :: TypeTable -> CBlockItem -> Codegen ()
generateBlockItem ts (CBlockStmt stat) = generateStatement ts stat
generateBlockItem _ _ = trace "unknown generate block item: " $ undefined

generateIfStatement :: TypeTable -> CExpr -> CStat -> Maybe CStat -> Codegen ()
generateIfStatement ts condition trueStat maybeElseStat = do
	ifthen <- addBlock "if.then"
	ifelse <- addBlock "if.else"
	ifexit <- addBlock "if.exit"

	-- %entry
	------------------
	(cond, _, _) <- generateExpression ts condition
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
		Nothing -> return ()

	-- if.exit
	------------------
	setBlock ifexit
	-- will this work? do we need this at all?
	return ()