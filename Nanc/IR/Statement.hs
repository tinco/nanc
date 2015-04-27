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

generateStatement :: CStat -> Codegen ()
generateStatement (CExpr expr _) = void $ generateExpression (fromJust expr)
generateStatement (CReturn Nothing _)= void $ ret Nothing
generateStatement (CReturn (Just expr) _) = void $ generateExpression expr >>= ret . Just . fst
generateStatement (CCompound _ident items _) = mapM_ generateBlockItem items
generateStatement (CIf expr trueStat maybeElseStat _) = generateIfStatement expr trueStat maybeElseStat
generateStatement _d = trace ("Unknown generateStatement: " ++ show _d) $ undefined

generateBlockItem :: CBlockItem -> Codegen ()
generateBlockItem (CBlockStmt stat) = generateStatement stat
generateBlockItem _ = trace "unknown generate block item" $ undefined

generateIfStatement :: CExpr -> CStat -> Maybe CStat -> Codegen ()
generateIfStatement condition trueStat maybeElseStat = do
	ifthen <- addBlock "if.then"
	ifelse <- addBlock "if.else"
	ifexit <- addBlock "if.exit"

	-- %entry
	------------------
	(cond, _) <- generateExpression condition
	cbr cond ifthen ifelse

	-- if.then
	------------------
	setBlock ifthen
	generateStatement trueStat
	br ifexit
	ifthen <- getBlock

	-- if.else
	------------------
	case maybeElseStat of
		Just elseState -> do
			setBlock ifelse
			generateStatement elseState
			br ifexit
			ifelse <- getBlock
			return ()
		Nothing -> return ()

	-- if.exit
	------------------
	setBlock ifexit
	-- will this work? do we need this at all?
	return ()