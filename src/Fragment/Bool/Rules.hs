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
module Fragment.Bool.Rules (
    RBool
  ) where

import Rules
import Ast.Error.Common

import Fragment.KiBase.Ast.Kind

import Fragment.Bool.Ast
import qualified Fragment.Bool.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Bool.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Bool.Rules.Type.Infer.Offline as TUO
import Fragment.Bool.Rules.Type
import Fragment.Bool.Rules.Term

data RBool

instance RulesIn RBool where
  type InferKindContextSyntax e w s r m ki ty a RBool = KSD.BoolInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RBool = TSD.BoolInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RBool = TUO.BoolInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RBool = BoolTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RBool = BoolTermContext ki ty pt tm a
  type KindList RBool = '[KiFBase]
  type TypeList RBool = '[TyFBool]
  type ErrorList ki ty pt tm a RBool = '[ErrUnexpectedType ki ty a]
  type WarningList ki ty pt tm a RBool = '[]
  type PatternList RBool = '[PtFBool]
  type TermList RBool = '[TmFBool]

  inferKindInputSyntax _ = KSD.boolInferKindRules
  inferTypeInputSyntax _ = TSD.boolInferTypeRules
  inferTypeInputOffline _ = TUO.boolInferTypeRules
  typeInput _ = boolTypeRules
  termInput _ = boolTermRules

