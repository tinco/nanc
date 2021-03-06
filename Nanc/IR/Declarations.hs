module Nanc.IR.Declarations where

import Debug.Trace
import Data.Maybe
import Data.List

import Control.Monad.State

import Language.C
import Language.C.Data.Ident

import qualified LLVM.General.AST as AST
import LLVM.General.AST.Global as ASTG
import LLVM.General.AST.Operand
import qualified LLVM.General.AST.Constant as C

import Nanc.CodeGenState
import Nanc.AST
import Nanc.AST.Declarations

import Nanc.IR.Types
import Nanc.IR.Statement
import Nanc.IR.Expression
import Nanc.IR.Instructions

{-
C99 requires that there is at least one specifier, though this is merely a syntactic restriction
at most one storage class specifier is allowed per declaration
the elements of the non-empty init-declarator-list are of the form (Just declr, init?, Nothing). The declarator declr has to be present and non-abstract and the initialization expression is optional.
-}

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

{-
 Declaration {declarationName = "__ino_t",
 	 declarationSpecs = DeclarationSpecs {
 	 	declStorage = Typedef,
 	 	declType = QualifiedType (ST UnsignedLongInt) (TypeQualifiers {typeIsVolatile = False, typeIsConst = False, typeIsRestrict = False, typeIsInline = False}),
        declStorageNodes = [(NodeInfo ("/usr/include/x86_64-linux-gnu/bits/types.h": line 127) (("/usr/include/x86_64-linux-gnu/bits/types.h": line 127),7) Name {nameId = 117})],
        declTypeNodes = [(NodeInfo ("/usr/include/x86_64-linux-gnu/bits/types.h": line 127) (("/usr/include/x86_64-linux-gnu/bits/types.h": line 127),8) Name {nameId = 118})], declQualifierNodes = []},
        declarationType = QualifiedType (ST UnsignedLongInt) (TypeQualifiers {typeIsVolatile = False, typeIsConst = False, typeIsRestrict = False, typeIsInline = False})}
-}

{-
Typedef: CDecl 
		[CStorageSpec (CTypedef),
			CTypeSpec (CSUType 
				(CStruct CStructTag 
					(Just (Ident "_IO_FILE" ) 
		[(Just (CDeclr (Just (Ident "__FILE" 154260139

-}

generateTypedef :: Declaration -> Module ()
generateTypedef declaration = do
		defs <- gets typeDefinitions
		modify $ \s -> s { typeDefinitions = defs ++ [(name, typ)] }
	where
		name = declarationName declaration
		typ = declarationType declaration
		--typ = trace ("Typedef typ = " ++ (show $ declarationType declaration) ++ " of name " ++ (show name) ++ "\n") $ declarationType declaration

resolveTypeDefinitions :: Module ()
resolveTypeDefinitions = do
	typeDefs' <- gets typeDefinitions
	let typeDefs = resolveForwardDefinitions typeDefs'
	let (aliases, direct) = partition (isTypeAlias.snd) typeDefs
	let (result, _) = resolveTypeAliases direct aliases
	modify $ \s -> s { typeDefinitions = result }
	-- uncomment next line to show list of types that are going to be compiled
	-- traceShowM $ map fst result
	-- use tail of result because we dont want va_list to be in there
	mapM_ (addTypeDefn result) (tail result)
	where
		resolveForwardDefinitions :: TypeTable -> TypeTable
		resolveForwardDefinitions (t@(n, _):ts) = case lookup n ts of
			-- TODO maybe check whether the forward definition matches its full definition?
			Just _ -> resolveForwardDefinitions ts
			Nothing -> t : resolveForwardDefinitions ts
		resolveForwardDefinitions ts = ts
		resolveTypeAliases :: TypeTable -> TypeTable -> (TypeTable, TypeTable)
		resolveTypeAliases direct [] = (direct, [])
		resolveTypeAliases direct ((e@(n, QualifiedType (TypeAlias aliasName) _)):aliases) = case lookup aliasName direct of
			Just t -> resolveTypeAliases ((n,t):direct) aliases
			-- HACK: omg maybe check if the first 7 are "struct " first?
			Nothing -> case lookup (drop (length "struct ") aliasName) direct of
				Just t -> resolveTypeAliases ((n,t):direct) aliases
				Nothing -> trace ("Can't find type for definition: " ++ aliasName ++ " " ++ (show e) ++ "\n" ++ (show direct)) $ undefined

generateStaticVariable :: Declaration -> Module ()
generateStaticVariable decl = do
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

-- buildGlobalSymbolTable :: [GlobalDeclaration] -> SymbolTable
buildGlobalSymbolTable :: [(String, QualifiedType, AST.Definition)] -> SymbolTable
buildGlobalSymbolTable [] = []
buildGlobalSymbolTable ((name,qt,AST.GlobalDefinition gd):rest) = (b gd): buildGlobalSymbolTable rest
	where
		b :: Global -> (String, Symbol)
		b (GlobalVariable n _ _ _ _ _ _ _ t _ _ _ _) = (name, (ConstantOperand $ C.GlobalReference t n, qt))
		b (GlobalAlias (AST.Name name) _ _ _ _ _ _ _) = trace ("unsupported global alias: " ++ name) (name, undefined)
		-- We don't have access to the (full) typetable here, so we have to be super awkward about building the function type
		b f@(Function _ _ _ _ _ t n _ _ _ _ _ _ _ _) = (name, (ConstantOperand $ C.GlobalReference (buildFunctionType f) n, qt))
{- GlobalVariable Name Linkage Visibility Bool AddrSpace Bool Bool Type (Maybe Constant) (Maybe String) Word32	
   GlobalAlias Name Linkage Visibility Type Constant	
   Function Linkage Visibility CallingConvention [ParameterAttribute] Type Name ([Parameter], Bool) [FunctionAttribute] (Maybe String) Word32 (Maybe String) [BasicBlock] -}

-- for now skip over typedefs in symboltable
buildGlobalSymbolTable ((_name, _qt, AST.TypeDefinition (AST.Name _n) _maybeType):rest) = trace ("skipping: " ++ (show _n)) $ buildGlobalSymbolTable rest

buildFunctionType :: Global -> AST.Type
buildFunctionType f = AST.FunctionType (ASTG.returnType f) (map parameterType (fst $ parameters f)) (snd $ parameters f)
	where
		parameterType (AST.Parameter t _ _) = t

{- GlobalDefinition Global	 
   TypeDefinition Name (Maybe Type)	 
   MetadataNodeDefinition MetadataNodeID [Maybe Operand]	 
   NamedMetadataDefinition String [MetadataNodeID]	 
   ModuleInlineAssembly String -}

generateFunDef :: CFunDef -> Module ()
generateFunDef (CFunDef specs declr _decls stat _) = do
		globalDecls <- gets globalDeclarations
		typeDefs <- gets typeDefinitions
		litsCount <- gets globalLiteralsCount
		let defs = globalDecls
		let fnargs = extractFnArgs typeDefs declr
		let params = map (\d -> (declarationType d, declarationName d)) fnargs
		let retTyp = declType declSpecs
		let typ = QualifiedType (FT (FunctionType retTyp params)) defaultTypeQualifiers
		-- let argumentSymbols = map (\ (d) -> (declarationName d, (LocalReference (qualifiedTypeToType typeDefs $ declarationType d) (AST.Name (declarationName d)), declarationType d))) fnargs

		let initialCodeGenState ds = (emptyCodegen litsCount) {
			symboltables = [buildGlobalSymbolTable ds]
		}
		let cg = execCodegen (initialCodeGenState defs) $ do
			entryB <- addBlock entryBlockName
			setBlock entryB

			-- HACK: we store our params in a temporary variable that gets
			-- optimized out by llvm, this is needed because we currently
			-- have no way of referring directly values in the symboltable
			-- (it can only store adresses)
			let declareParam (typ, name) = do
				let t = qualifiedTypeToType typeDefs typ
				let arg = LocalReference t (AST.Name name)
				tmp <- alloca t
				store t tmp arg
				assign name (tmp, typ)

			mapM declareParam params

			generateStatement typeDefs stat

			if name == "main" then do
				maybeReturnZero
			else
				return ()

		let litC = literalsCount cg
		let lits = literals cg

		defineLiterals litC lits

		let bls ds = createBlocks cg
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
extractDeclrName (CDeclr ident _ _ _ m) = maybe ("Anon" ++ (show $ metaToRandom m)) (\ (Ident n _ _) -> n) ident

extractFnArgs :: TypeDefinitions -> CDeclr -> [Declaration]
-- Only parse new style params
extractFnArgs typeDefs (CDeclr _  ((CFunDeclr (Right (params, mysteriousBool)) _ _):_) _ _ _) = map buildDeclaration params
extractFnArgs typeDefs declrs = trace ("Dont know how to extract fnargs from: " ++ (show declrs)) undefined

declsToFnArgs :: TypeTable -> [Declaration] -> [(AST.Type, AST.Name)]
-- Only parse new style params
declsToFnArgs ts decls = map (\ d -> (qualifiedTypeToType ts $ declarationType d, AST.Name $ declarationName d) ) decls
