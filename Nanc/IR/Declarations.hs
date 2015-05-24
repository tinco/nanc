module Nanc.IR.Declarations where

import Debug.Trace
import Data.Maybe

import Control.Monad.State

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST
import LLVM.General.AST.Global
import LLVM.General.AST.Operand
import qualified LLVM.General.AST.Constant as C

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Statement
import Nanc.IR.Expression

-- TODO: why compile declarations we might never reference? We should
-- make this lazy. So a declaration only gets compiled when it's referenced
-- from main or is externally visible.

{-
External declarations can either be a toplevel declaration, a function definition or external assember
-}
generateExtDecl :: CExtDecl -> Module ()
generateExtDecl (CDeclExt decl) = generateToplevelDecl decl
generateExtDecl (CFDefExt decl) = generateFunDef decl
generateExtDecl (CAsmExt _decl _) = trace "ASM" $ undefined

{-
C99 requires that there is at least one specifier, though this is merely a syntactic restriction
at most one storage class specifier is allowed per declaration
the elements of the non-empty init-declarator-list are of the form (Just declr, init?, Nothing). The declarator declr has to be present and non-abstract and the initialization expression is optional.
-}
generateToplevelDecl :: CDecl -> Module ()
generateToplevelDecl decl
	| isExtern && isFunction = trace "ExternFunction" $ generateExternFunction declaration
	| isExtern = trace "ExternVariable" $ generateExternVariable declaration
	| isTypedef = trace "Typedef" $ generateTypedef declaration
	| isStatic = trace "StaticDecl" $ generateStaticDecl declaration
	| otherwise = trace ("got unknown toplevel decl: " ++ (show declaration)) undefined
	where
		declaration = globalDeclarationDefaults $ buildDeclaration decl
		storage = declStorage $ declarationSpecs declaration
		isExtern = storage == Extern
		isTypedef = storage == Typedef
		isStatic = storage == Static
		isFunction = isFunctionType $ declarationType declaration

generateExternFunction :: Declaration -> Module ()
generateExternFunction declaration = do
		defs <- gets typeDefinitions
		let fTypeToTuple (FT (FunctionType rt argts)) = (qualifiedTypeToType defs rt, map (\ (t,n) -> (qualifiedTypeToType defs t, AST.Name n)) argts) 
		let (retty, argtys) = fTypeToTuple fType
		external retty name argtys
	where
		fType = qualifiedTypeType $ declarationType declaration
		name = declarationName declaration
	
generateExternVariable :: Declaration -> Module ()
generateExternVariable declaration = trace ("Non function extern: " ++ (show $ declarationName declaration)) $ return ()

generateTypedef :: Declaration -> Module ()
generateTypedef declaration = do
		return $ trace "Generate type def"
		defs <- gets typeDefinitions
		modify $ \s -> s { typeDefinitions = defs ++ [(name, declarationType declaration)] }
		return $ trace name ()
	where
		name = declarationName declaration	

generateStaticDecl :: Declaration -> Module ()
generateStaticDecl decl = do
		defs <- gets typeDefinitions
		let def = AST.GlobalDefinition $ AST.globalVariableDefaults {
			name = AST.Name $ declarationName decl,
			type' = qualifiedTypeToType defs $ declType $ declarationSpecs decl
		}
		addDefn def

buildGlobalSymbolTable :: [AST.Definition] -> [(String, AST.Operand)]
buildGlobalSymbolTable [] = []
buildGlobalSymbolTable ((AST.GlobalDefinition gd):rest) = (b gd): buildGlobalSymbolTable rest
	where
		b :: Global -> (String, AST.Operand)
		b (GlobalVariable n@(AST.Name name) _ _ _ _ _ _ t _ _ _) = (name, ConstantOperand $ C.GlobalReference t n)
		b (GlobalAlias (AST.Name name) _ _ _ _) = trace ("unsupported global alias: " ++ name) (name, undefined)
		b (Function _ _ _ _ t n@(AST.Name name) _ _ _ _ _ _) = (name, ConstantOperand $ C.GlobalReference t n)
{- GlobalVariable Name Linkage Visibility Bool AddrSpace Bool Bool Type (Maybe Constant) (Maybe String) Word32	
   GlobalAlias Name Linkage Visibility Type Constant	
   Function Linkage Visibility CallingConvention [ParameterAttribute] Type Name ([Parameter], Bool) [FunctionAttribute] (Maybe String) Word32 (Maybe String) [BasicBlock] -}

-- for now skip over typedefs in symboltable
buildGlobalSymbolTable ((AST.TypeDefinition (AST.Name _n) _maybeType):rest) = buildGlobalSymbolTable rest
{- GlobalDefinition Global	 
   TypeDefinition Name (Maybe Type)	 
   MetadataNodeDefinition MetadataNodeID [Maybe Operand]	 
   NamedMetadataDefinition String [MetadataNodeID]	 
   ModuleInlineAssembly String -}

generateFunDef :: CFunDef -> Module ()
generateFunDef (CFunDef specs declr _decls stat _) = do
		llvmModuleState <- gets llvmModuleState
		let defs = AST.moduleDefinitions llvmModuleState
		let tp = qualifiedTypeToType [] $ declType declSpecs
		define tp name fnargs (bls defs)
	where
		declSpecs = buildDeclarationSpecs specs
		name = extractDeclrName declr
		_args = []
		fnargs = []

		initialCodeGenState ds = emptyCodegen {
			symboltables = [buildGlobalSymbolTable ds]
		}

		bls ds = createBlocks $ execCodegen (initialCodeGenState ds) $ do
			entryB <- addBlock entryBlockName
			setBlock entryB
			-- generate argument code here
			generateStatement stat

extractDeclrName :: CDeclr -> String
extractDeclrName (CDeclr ident _ _ _ _)= maybe "anonymous" (\ (Ident n _ _) -> n) ident