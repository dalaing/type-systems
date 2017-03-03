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
module Fragment.Pair.Rules (
    RPair
  ) where

import Rules

import Fragment.KiBase.Ast.Kind

import Fragment.Pair.Ast
import qualified Fragment.Pair.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Pair.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Pair.Rules.Type.Infer.Offline as TUO
import Fragment.Pair.Rules.Type
import Fragment.Pair.Rules.Term

data RPair

instance RulesIn RPair where
  type RuleKindInferSyntaxContext e w s r m ki ty a RPair = KSD.PairKindRulesContext e w s r m ki ty a
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RPair = TSD.PairInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RPair = TUO.PairInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RPair = PairTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RPair = PairTermContext ki ty pt tm a
  type KindList RPair = '[KiFBase]
  type TypeList RPair = '[TyFPair]
  type ErrorList ki ty pt tm a RPair = '[ErrExpectedTyPair ki ty a]
  type WarningList ki ty pt tm a RPair = '[]
  type PatternList RPair = '[PtFPair]
  type TermList RPair = '[TmFPair]

  inferKindInputSyntax _ = KSD.pairKindRules
  inferSyntaxInput _ = TSD.pairInferRules
  inferOfflineInput _ = TUO.pairInferRules
  typeInput _ = pairTypeRules
  termInput _ = pairTermRules
