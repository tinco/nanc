{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nanc.CodeGenState where

import Nanc.AST

import Data.Word
import Data.List
import Data.Function

import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST hiding (Module)
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState {
	currentBlock :: Name,                    -- Name of the active block to append to
	blocks       :: Map.Map Name BlockState, -- Blocks for function
	symboltables :: [SymbolTable],           -- Function scope symbol table
	blockCount   :: Int,                     -- Count of basic blocks
	count        :: Word,                    -- Count of unnamed instructions
	names        :: Names                    -- Name Supply
} deriving Show

{-
 A function definition contains a list of basic blocks,
 forming the CFG (Control Flow Graph) for the function.
-}
data BlockState = BlockState {
	idx   :: Int,                            -- Block index
	stack :: [Named Instruction],            -- Stack of instructions
	term  :: Maybe (Named Terminator)        -- Block terminator
} deriving Show

data ModuleState = ModuleState {
	llvmModuleState :: AST.Module,
	typeDefinitions :: [(String, QualifiedType)]
}

newtype Codegen a = Codegen {
	runCodegen :: State CodegenState a
} deriving (Functor, Applicative, Monad, MonadState CodegenState )

newtype Module a = Module {
	unModule :: State ModuleState a
} deriving (Functor, Applicative, Monad, MonadState ModuleState )

runModule :: ModuleState -> Module a -> ModuleState
runModule = flip (execState . unModule)

execCodegen :: CodegenState -> Codegen a -> CodegenState
execCodegen initial m = execState (runCodegen m) initial

emptyModule :: String -> ModuleState
emptyModule label = ModuleState {
	llvmModuleState = defaultModule { moduleName = label },
	typeDefinitions = builtinTypeDefinitions
}

builtinTypeDefinitions :: [(String, QualifiedType)]
builtinTypeDefinitions = [
		("__builtin_va_list", QualifiedType (Ptr (QualifiedType (ST Char) defaultTypeQualifiers)) defaultTypeQualifiers)
	]

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

entryBlockName :: String
entryBlockName = "entry"

addDefn :: Definition -> Module ()
addDefn d = do
	llvmModuleState <- gets llvmModuleState
	let defs = moduleDefinitions llvmModuleState
	modify $ \s -> s { llvmModuleState = llvmModuleState { moduleDefinitions = defs ++ [d] } }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> Module ()
define retty label argtys body = addDefn $ GlobalDefinition $ functionDefaults {
		name        = Name label,
		parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
		returnType  = retty,
		basicBlocks = body
	}

external ::  Type -> String -> [(Type, Name)] -> Module ()
external retty label argtys = addDefn $	GlobalDefinition $ functionDefaults {
		name        = Name label,
		parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
		returnType  = retty,
		basicBlocks = []
	}

entry :: Codegen Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: String -> Codegen Name
addBlock bname = do
	bls <- gets blocks
	ix  <- gets blockCount
	nms <- gets names

	let new = emptyBlock ix 
	let (qname, supply) = uniqueName bname nms

	modify $ \s -> s {
		blocks = Map.insert (Name qname) new bls,
		blockCount = ix + 1,
		names = supply
	}

	return (Name qname)

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
	where
		maketerm (Just x) = x
		maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

setBlock :: Name -> Codegen Name
setBlock bname = do
	modify $ \s -> s { currentBlock = bname }
	return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
	active <- gets currentBlock
	modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
	c <- gets currentBlock
	blks <- gets blocks
	case Map.lookup c blks of
		Just x -> return x
		Nothing -> error $ "No such block: " ++ show c

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
	case Map.lookup nm ns of
		Nothing -> (nm,  Map.insert nm 1 ns)
		Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

fresh :: Codegen Word
fresh = do
	i <- gets count
	modify $ \s -> s { count = 1 + i }
	return $ i + 1

local ::  Name -> Type -> Operand
local n t = LocalReference t n

externf :: Name -> Type -> Operand
externf n t = ConstantOperand . C.GlobalReference t $ n

assign :: String -> Operand -> Codegen ()
assign var x = do
	symtabs <- gets symboltables
	modify $ \s -> s { symboltables = ([(var, x)] ++ (head symtabs)) : (tail symtabs) }

symbolLookup :: String -> [SymbolTable] -> Maybe Operand
symbolLookup name (t:rest) = case lookup name t of
	Just operand -> Just operand
	Nothing -> symbolLookup name rest
symbolLookup _ [] = Nothing

getvar :: String -> Codegen Operand
getvar var = do
  symtabs <- gets symboltables
  case symbolLookup var symtabs of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var