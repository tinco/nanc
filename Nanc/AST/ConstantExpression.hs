module Nanc.AST.ConstantExpression where

import Debug.Trace

import Data.Word

import Language.C

intValue :: CExpr -> Word64
intValue (CConst (CIntConst i _)) = fromIntegral $ getCInteger i
intValue expr = trace ("Unknown ConstantIntExpression: " ++ (show expr)) undefined