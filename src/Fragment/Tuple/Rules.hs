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
module Fragment.Tuple.Rules (
    RTuple
  ) where

import Rules

import Fragment.KiBase.Ast.Kind

import Fragment.Tuple.Ast
import qualified Fragment.Tuple.Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Fragment.Tuple.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Tuple.Rules.Type.Infer.Offline as TUO
import Fragment.Tuple.Rules.Type
import Fragment.Tuple.Rules.Term

data RTuple

instance RulesIn RTuple where
  type RuleKindInferSyntaxContext e w s r m ki ty a RTuple = KSD.TupleKindRulesContext e w s r m ki ty a
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RTuple = TSD.TupleInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RTuple = TUO.TupleInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RTuple = TupleTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RTuple = TupleTermContext ki ty pt tm a
  type KindList RTuple = '[KiFBase]
  type TypeList RTuple = '[TyFTuple]
  type ErrorList ki ty pt tm a RTuple = '[ErrExpectedTyTuple ki ty a, ErrTupleOutOfBounds]
  type WarningList ki ty pt tm a RTuple = '[]
  type PatternList RTuple = '[PtFTuple]
  type TermList RTuple = '[TmFTuple]

  inferKindInputSyntax _ = KSD.tupleKindRules
  inferSyntaxInput _ = TSD.tupleInferRules
  inferOfflineInput _ = TUO.tupleInferRules
  typeInput _ = tupleTypeRules
  termInput _ = tupleTermRules
