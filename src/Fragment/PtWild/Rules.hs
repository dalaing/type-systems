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
module Fragment.PtWild.Rules (
    RPtWild
  ) where

import GHC.Exts (Constraint)

import Rules
import Ast.Pattern

import qualified Fragment.PtWild.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.PtWild.Rules.Type.Infer.Offline as UO
import Fragment.PtWild.Rules.Term

data RPtWild

instance RulesIn RPtWild where
  type InferKindContextSyntax e w s r m ki ty a RPtWild = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RPtWild = SD.PtWildInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RPtWild = UO.PtWildInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RPtWild = (() :: Constraint)
  type RuleTermContext ki ty pt tm a RPtWild = PtWildTermContext ki ty pt tm a
  type KindList RPtWild = '[]
  type TypeList RPtWild = '[]
  type ErrorList ki ty pt tm a RPtWild = '[]
  type WarningList ki ty pt tm a RPtWild = '[]
  type PatternList RPtWild = '[PtFWild]
  type TermList RPtWild = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = SD.ptWildInferTypeRules
  inferTypeInputOffline _ = UO.ptWildInferTypeRules
  typeInput _ = mempty
  termInput _ = ptWildTermRules
