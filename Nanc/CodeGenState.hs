{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nanc.CodeGenState where

import Debug.Trace
import GHC.Stack
import Text.Show.Pretty (ppShow)

import Nanc.AST hiding (returnType)

import Data.Word
import Data.List
import Data.Function
import Data.Maybe
import Data.Either

import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import qualified Language.C as LC

import LLVM.General.AST hiding (Module)
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as LT
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Linkage as L

import Nanc.IR.Types

type Symbol = (Operand, QualifiedType)
type SymbolTable = [(String, Symbol)]
type TypeTable = [(String, QualifiedType)]
type GlobalDeclaration = (String, QualifiedType, Definition)

data SwitchContext = SwitchContext {
	cases :: [(LC.CExpr, Name {- entry -}, Name {- exit -})],
	defaultCase :: Name
} deriving Show

data CodegenState = CodegenState {
	currentBlock :: Name,                    -- Name of the active block to append to
	blocks       :: Map.Map Name BlockState, -- Blocks for function
	loopEntryStack :: [Name],                -- Entries of loops that are currently nested (for continue)
	loopExitStack :: [Name],                 -- Exits of loops that are currently nested (for break)
	switchStack :: [SwitchContext],          -- Context of switches that we are currently nested in
	symboltables :: [SymbolTable],           -- Function scope symbol table
	blockCount   :: Int,                     -- Count of basic blocks
	count        :: Word,                    -- Count of unnamed instructions
	names        :: Names,                   -- Name Supply
	literalsCount :: Int,                   -- Count of literals
	literals     :: [(String, QualifiedType, C.Constant)]  -- Literals that need to be declared globally
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
	typeDefinitions :: TypeTable,
	globalDeclarations :: [GlobalDeclaration],
	globalLiteralsCount :: Int
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
	typeDefinitions = builtinTypeDefinitions,
	globalDeclarations = [],
	globalLiteralsCount = 0
}

{- More information about builtins here:
 - https://github.com/llvm-mirror/clang/blob/master/lib/CodeGen/CGBuiltin.cpp#L236
 - <o11c> d-snp: on GCC you can grep for TARGET_BUILD_BUILTIN_VA_LIST
 - <jroelofs> d-snp: the frontend gets them from here: https://github.com/llvm-mirror/clang/blob/master/lib/Headers/stdarg.h#L33
 - ASTContext.cpp -> CreateVaListDecl
 - < o11c> d-snp: https://raw.githubusercontent.com/gcc-mirror/gcc/master/gcc/config/i386/i386.c    and grep for    ix86_build_builtin_va_list_abi
 -}
builtinTypeDefinitions :: TypeTable
builtinTypeDefinitions = [
		("__builtin_va_list", QualifiedType (Ptr (QualifiedType (ST Char) defaultTypeQualifiers)) defaultTypeQualifiers)
	]

emptyCodegen :: Int -> CodegenState
emptyCodegen litsCount = CodegenState {
	switchStack = [],
	currentBlock = (Name entryBlockName),
	blocks = Map.empty,
	loopEntryStack = [],
	loopExitStack = [],
	symboltables = [],
	blockCount = 1,
	count = 0,
	names = Map.empty,
	literalsCount = litsCount,
	literals = []
}


entryBlockName :: String
entryBlockName = "entry"

addTypeDefn :: TypeTable -> (String, QualifiedType) -> Module()
addTypeDefn ts (n,t) 
	| isStructType t = do
		let defn = AST.TypeDefinition (AST.Name n) (Just $ qualifiedTypeToType ts t)
		llvmModuleState <- gets llvmModuleState
		let defs = moduleDefinitions llvmModuleState
		modify $ \s -> s { llvmModuleState = llvmModuleState { moduleDefinitions = defs ++ [defn] } }
	-- other types can't be named types
	| otherwise = return ()

addDefn :: String -> QualifiedType -> Definition -> Module ()
addDefn n qt d = do
	globalDefs <- gets globalDeclarations
	llvmModuleState <- gets llvmModuleState
	let defs = moduleDefinitions llvmModuleState
	modify $ \s -> s { llvmModuleState = llvmModuleState { moduleDefinitions = defs ++ [d] },
	                   globalDeclarations = globalDefs ++ [(n, qt, d)] }

defineFunction :: TypeTable -> String -> QualifiedType -> [BasicBlock] -> Module ()
defineFunction ts name qt@(QualifiedType (FT (Nanc.AST.FunctionType retty params)) _) body = addDefn name qt definition
	where
		definition = GlobalDefinition $ functionDefaults {
				name        = Name name,
				parameters  = ([Parameter (qualifiedTypeToType ts ty) (Name nm) [] | (ty, nm) <- params], False),
				returnType  = qualifiedTypeToType ts retty,
				basicBlocks = body
			}

external ::  TypeTable -> String -> QualifiedType -> Module ()
external ts name qt = defineFunction ts name qt []

defineLiterals :: Int -> [(String, QualifiedType, C.Constant)] -> Module ()
defineLiterals idx lits = do
	defs <- gets typeDefinitions
	let makeLit name qt c = AST.GlobalDefinition $ AST.globalVariableDefaults {
			name = Name name,
			LLVM.General.AST.Global.type' = qualifiedTypeToType defs qt,
			linkage = L.Private,
			hasUnnamedAddr = True,
			isConstant = True,
			initializer = Just c
	}
	mapM (\(name, qt, c) -> addDefn name qt (makeLit name qt c)) lits
	modify $ \s -> s { globalLiteralsCount = idx }
	where


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
createBlocks m = if hasInvalidBlock
	then error $ "Has block without terminator: " ++ (ppShow maybeBlocks)
	else lefts maybeBlocks
	where
		hasInvalidBlock = any isRight maybeBlocks
		maybeBlocks = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> Either BasicBlock String
makeBlock b@(l, (BlockState _ s t)) = case t of
	Just term -> Left $ BasicBlock l s term
	Nothing -> Right $ "No terminator for block: " ++ (ppShow b)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

setBlock :: Name -> Codegen Name
setBlock bname = do
	modify $ \s -> s { currentBlock = bname }
	return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

-- TODO actually check whether the block exists
findBlock :: String -> Codegen Name
findBlock = return . Name

-- Pushes loop entry and exit block names unto the current loop stack
-- these blocks are used for the 'break' and 'continue' statements
-- should be called when a loop is entered (i.e. 'for' and 'while')
-- Entry should be the name of the block 'continue' would jump to
-- Exit should be the name of the block 'break' would jump to
pushLoop :: Name -> Name -> Codegen ()
pushLoop entry exit =
	modify $ \s -> s {
		loopEntryStack = entry : (loopEntryStack s),
		loopExitStack = exit : (loopExitStack s)
	}

-- Pops loop entry and exit blocks. Should be called when a loop
-- is exited.
popLoop :: Codegen ()
popLoop =
	modify $ \s -> s {
		loopEntryStack = tail $ loopEntryStack s,
		loopExitStack = tail $ loopEntryStack s
	}

pushSwitch :: SwitchContext -> Codegen ()
pushSwitch ctx =
	modify $ \s -> s {
		switchStack = ctx : (switchStack s)
	}

popSwitch :: Codegen SwitchContext
popSwitch = do
	stack <- gets switchStack
	modify $ \s -> s {
		switchStack = tail $ switchStack s
	}
	return $ head stack

addSwitchCase :: LC.CExpr -> Name -> Name -> Codegen ()
addSwitchCase c n e = do
	stack <- gets switchStack
	let cases' = (c,n,e) : (cases $ head stack)
	let headStack' = (head stack) { cases = cases' }
	modify $ \s -> s {
		switchStack = headStack' : (tail stack)
	}

getSwitchDefault :: Codegen Name
getSwitchDefault = do
	stack <- gets switchStack
	return $ defaultCase $ head stack

-- Gets the entrypoint of the current loop, should be used as the target
-- for the 'continue' statement.
currentLoopEntry :: Codegen Name
currentLoopEntry = do 
	stack <- gets loopEntryStack
	return $ head stack

-- Gets the exit block of the current loop, should be used as the target
-- for the 'break' statement.
currentLoopExit :: Codegen Name
currentLoopExit = do 
	stack <- gets loopExitStack
	return $ head stack

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
	active <- gets currentBlock
	modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
	c <- gets currentBlock
	lookupBlockState c

lookupBlockState :: Name -> Codegen BlockState
lookupBlockState name = do
	blocks <- gets blocks
	case Map.lookup name blocks of
		Just bs -> return bs
		Nothing -> error $ "Could not find block named: " ++ show name

hasTerminator :: BlockState -> Bool
hasTerminator bs = isJust $ term bs

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

freshLiteral :: Codegen Int
freshLiteral = do
	i <- gets literalsCount
	modify $ \s -> s { literalsCount = 1 + i }
	return $ i + 1

local ::  Name -> Type -> Operand
local n t = LocalReference t n

global :: Name -> Type -> Operand
global n t = ConstantOperand . C.GlobalReference t $ n

assign :: String -> Symbol -> Codegen ()
assign var x = do
	symtabs <- gets symboltables
	modify $ \s -> s { symboltables = ([(var, x)] ++ (head symtabs)) : (tail symtabs) }

symbolLookup :: String -> [SymbolTable] -> Maybe Symbol
symbolLookup name (t:rest) = case lookup name t of
	Just symbol -> Just symbol
	Nothing -> symbolLookup name rest
symbolLookup _ [] = Nothing

literal :: (C.Constant, QualifiedType) -> Codegen (String)
literal (c,t) = do
	idx <- freshLiteral
	let name = "___lit_" ++ (show idx)
	lits <- gets literals
	modify $ \s -> s { literals = lits ++ [(name, t, c)] }
	return name

getvar :: String -> Codegen Symbol
getvar var = do
  symtabs <- gets symboltables
  case symbolLookup var symtabs of
    Just x  -> return x
    Nothing -> errorWithStackTrace $ "Local variable not in scope: " ++ (show var) ++ " symtable: " ++ (show symtabs)
