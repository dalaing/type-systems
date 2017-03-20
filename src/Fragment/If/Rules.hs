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
module Fragment.If.Rules (
    RIf
  ) where

import GHC.Exts (Constraint)

import Ast
import Ast.Error.Common
import Rules

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Term

import Fragment.If.Ast
import qualified Fragment.If.Rules.Type.Infer.SyntaxDirected as SD
import qualified Fragment.If.Rules.Type.Infer.Offline as UO
import Fragment.If.Rules.Term

data RIf

instance AstIn RIf where
  type KindList RIf = '[]
  type TypeList RIf = '[TyFBool]
  type PatternList RIf = '[]
  type TermList RIf = '[TmFBool, TmFIf]

instance RulesIn RIf where
  type InferKindContextSyntax e w s r m ki ty a RIf = (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RIf = SD.IfInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RIf = UO.IfInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RIf = (() :: Constraint)
  type RuleTermContext ki ty pt tm a RIf = IfTermContext ki ty pt tm a
  type ErrorList ki ty pt tm a RIf = '[ErrUnexpectedType ki ty a, ErrExpectedTypeEq ki ty a]
  type WarningList ki ty pt tm a RIf = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = SD.ifInferTypeRules
  inferTypeInputOffline _ = UO.ifInferTypeRules
  typeInput _ = mempty
  termInput _ = ifTermRules
