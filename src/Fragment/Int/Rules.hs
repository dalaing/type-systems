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
module Fragment.Int.Rules (
    RInt
  ) where

import Rules
import Ast.Error.Common

import Fragment.KiBase.Ast.Kind

import Fragment.Int.Ast
import qualified Fragment.Int.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Int.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Int.Rules.Type.Infer.Offline as TUO
import Fragment.Int.Rules.Type
import Fragment.Int.Rules.Term

data RInt

instance RulesIn RInt where
  type InferKindContextSyntax e w s r m ki ty a RInt = KSD.IntInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RInt = TSD.IntInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RInt = TUO.IntInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RInt = IntTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RInt = IntTermContext ki ty pt tm a
  type KindList RInt = '[KiFBase]
  type TypeList RInt = '[TyFInt]
  type ErrorList ki ty pt tm a RInt = '[ErrUnexpectedType ki ty a]
  type WarningList ki ty pt tm a RInt = '[]
  type PatternList RInt = '[PtFInt]
  type TermList RInt = '[TmFInt]

  inferKindInputSyntax _ = KSD.intInferKindRules
  inferTypeInputSyntax _ = TSD.intInferTypeRules
  inferTypeInputOffline _ = TUO.intInferTypeRules
  typeInput _ = intTypeRules
  termInput _ = intTermRules
