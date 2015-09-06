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
generateExtDecl (CAsmExt _decl _) = trace "ASM:" $ undefined

{-
C99 requires that there is at least one specifier, though this is merely a syntactic restriction
at most one storage class specifier is allowed per declaration
the elements of the non-empty init-declarator-list are of the form (Just declr, init?, Nothing). The declarator declr has to be present and non-abstract and the initialization expression is optional.
-}
generateToplevelDecl :: CDecl -> Module ()
generateToplevelDecl decl
	| isExtern && isFunction = trace "ExternFunction" $ generateExternFunction declaration
	| isExtern = trace "ExternVariable" $ generateExternVariable declaration
	| isTypedef = trace "TypeDef" $ generateTypedef declaration
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
		external defs name (declarationType declaration)
	where
		fType = qualifiedTypeType $ declarationType declaration
		name = declarationName declaration
	
generateExternVariable :: Declaration -> Module ()
generateExternVariable declaration = trace ("Non function extern: " ++ (show $ declarationName declaration)) $ return ()

generateTypedef :: Declaration -> Module ()
generateTypedef declaration = do
		defs <- gets typeDefinitions
		modify $ \s -> s { typeDefinitions = defs ++ [(name, declarationType declaration)] }
	where
		name = declarationName declaration	

generateStaticDecl :: Declaration -> Module ()
generateStaticDecl decl = do
		defs <- gets typeDefinitions
		let (def,name) = case typ of
			(QualifiedType (CT (E (CEnum (Just (Ident n _ _)) _ _ a))) _)
				->
					(AST.GlobalDefinition $ AST.globalVariableDefaults {
						name = AST.Name n,
						type' = qualifiedTypeToType defs $ typ
					}, n)
			_
				->
					(AST.GlobalDefinition $ AST.globalVariableDefaults {
						name = AST.Name $ declarationName decl,
						type' = qualifiedTypeToType defs $ typ
					}, declarationName decl)
		addDefn name typ def
	where
		typ = declType $ declarationSpecs decl

buildGlobalSymbolTable :: [AST.Definition] -> [(String, Symbol)]
buildGlobalSymbolTable [] = []
buildGlobalSymbolTable ((AST.GlobalDefinition gd):rest) = (b gd): buildGlobalSymbolTable rest
	where
		b :: Global -> (String, Symbol)
		b (GlobalVariable n@(AST.Name name) _ _ _ _ _ _ t _ _ _) = trace "GlobalVariables are going wrong:" $ (name, (ConstantOperand $ C.GlobalReference t n, undefined))
		b (GlobalAlias (AST.Name name) _ _ _ _) = trace ("unsupported global alias: " ++ name) (name, undefined)
		b (Function _ _ _ _ t n@(AST.Name name) _ _ _ _ _ _) = trace "Function variables are going wrong: " $ (name, (ConstantOperand $ C.GlobalReference t n, undefined))
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
		typeDefs <- gets typeDefinitions
		let defs = AST.moduleDefinitions llvmModuleState
		let fnargs = extractFnArgs typeDefs declr
		let params = map (\d -> (declarationType d, declarationName d)) fnargs
		let retTyp = declType declSpecs
		let typ = QualifiedType (FT (FunctionType retTyp params)) defaultTypeQualifiers
		let argumentSymbols = map (\ (d) -> (declarationName d, (LocalReference (qualifiedTypeToType typeDefs $ declarationType d) (AST.Name (declarationName d)), declarationType d))) fnargs
		let initialCodeGenState ds = emptyCodegen {
			symboltables = [argumentSymbols, buildGlobalSymbolTable ds]
		}
		let bls ds = createBlocks $ execCodegen (initialCodeGenState ds) $ do
			entryB <- addBlock entryBlockName
			setBlock entryB
			generateStatement typeDefs stat

			if name == "main" then do
				maybeReturnZero
			else
				return ()

		defineFunction typeDefs name typ (bls defs)
	where
		declSpecs = buildDeclarationSpecs specs
		name = extractDeclrName declr

maybeReturnZero = do
	c <- current
	case c of
		(BlockState _ _ Nothing) -> zeroReturn
		_ -> return ()

extractDeclrName :: CDeclr -> String
extractDeclrName (CDeclr ident _ _ _ _) = maybe "anonymous" (\ (Ident n _ _) -> n) ident

extractFnArgs :: TypeDefinitions -> CDeclr -> [Declaration]
-- Only parse new style params
extractFnArgs typeDefs (CDeclr _ [CFunDeclr (Right (params, mysteriousBool)) _ _] _ _ _) = map buildDeclaration params

declsToFnArgs :: TypeTable -> [Declaration] -> [(AST.Type, AST.Name)]
-- Only parse new style params
declsToFnArgs ts decls = map (\ d -> (qualifiedTypeToType ts $ declarationType d, AST.Name $ declarationName d) ) decls
