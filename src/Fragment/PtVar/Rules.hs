{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.PtVar.Rules (
    RPtVar
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules

import qualified Fragment.PtVar.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.PtVar.Rules.Type.Infer.Offline as UO

data RPtVar

instance AstIn RPtVar where
  type KindList RPtVar = '[]
  type TypeList RPtVar = '[]
  type PatternList RPtVar = '[]
  type TermList RPtVar = '[]

instance RulesIn RPtVar where
  type InferKindContextSyntax e w s r m ki ty a RPtVar = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RPtVar = SD.PtVarInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RPtVar = UO.PtVarInferTypeContext e w s r m ki ty pt tm a
  type ErrorList ki ty pt tm a RPtVar = '[]
  type WarningList ki ty pt tm a RPtVar = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = SD.ptVarInferTypeRules
  inferTypeInputOffline _ = UO.ptVarInferTypeRules
