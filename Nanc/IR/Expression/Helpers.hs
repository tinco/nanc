module Nanc.IR.Expression.Helpers where

import Debug.Trace

import Data.List
import Data.Word

import Nanc.AST
import Nanc.CodeGenState
import Nanc.AST.Declarations

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as LF

lookupMember :: TypeTable -> QualifiedType -> String -> (Int, QualifiedType)
lookupMember ts typ memName = (i, resultType)
	where
		members = extractMembers ts typ
		i = head $ elemIndices memName $ map (declarationName) members
		resultType = declarationType $ members !! i

-- Extracts the members from a Struct, TD or Ptr to a Struct
extractMembers :: TypeTable -> QualifiedType -> [Declaration]
extractMembers ts (QualifiedType (CT (Struct _ members _)) _) = members
extractMembers ts (QualifiedType (CT (TD n)) _)  = case lookup n ts of
	Just t -> extractMembers ts t
	Nothing -> trace ("Could not find struct type: " ++ (show n)) undefined
-- this doesn't really make sense..
extractMembers ts (QualifiedType (Ptr s) _) = extractMembers ts s
extractMembers ts s = trace ("Unexptected struct type: " ++ (show s)) undefined

intConst :: Integer -> AST.Operand
intConst = intConst64

boolConst :: Integer -> AST.Operand
boolConst = AST.ConstantOperand . C.Int 1

gepIndex :: Integer -> AST.Operand
gepIndex = intConst32

intConst8 :: Integer -> AST.Operand
intConst8 = AST.ConstantOperand . C.Int 8

intConst32 :: Integer -> AST.Operand
intConst32 = AST.ConstantOperand . C.Int 32

intConst64 :: Integer -> AST.Operand
intConst64 = AST.ConstantOperand . C.Int 64

floatConst :: Double -> AST.Operand
floatConst = AST.ConstantOperand . C.Float . LF.Double

wordMax :: AST.Operand
wordMax = intConst64 $ fromIntegral (maxBound :: Word64) 

isInteger :: AST.Operand -> Bool
isInteger (AST.LocalReference (AST.IntegerType _) _) = True
isInteger (AST.ConstantOperand (C.Int _ _)) = True
isInteger _ = False

isFloat :: AST.Operand -> Bool
isFloat (AST.LocalReference (AST.FloatingPointType _ _) _) = True
isFloat (AST.ConstantOperand (C.Float _)) = True
isFloat _ = False
