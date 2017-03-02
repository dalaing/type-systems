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

import Fragment.Tuple.Ast
import qualified Fragment.Tuple.Rules.Infer.SyntaxDirected as SD
import qualified Fragment.Tuple.Rules.Infer.Unification.Offline as UO
import Fragment.Tuple.Rules.Type
import Fragment.Tuple.Rules.Term

data RTuple

instance RulesIn RTuple where
  type RuleInferSyntaxContext e w s r m ki ty pt tm a RTuple = SD.TupleInferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a RTuple = UO.TupleInferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RTuple = TupleTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RTuple = TupleTermContext ki ty pt tm a
  type KindList RTuple = '[]
  type TypeList RTuple = '[TyFTuple]
  type ErrorList ki ty pt tm a RTuple = '[ErrExpectedTyTuple ki ty a, ErrTupleOutOfBounds]
  type WarningList ki ty pt tm a RTuple = '[]
  type PatternList RTuple = '[PtFTuple]
  type TermList RTuple = '[TmFTuple]

  inferSyntaxInput _ = SD.tupleInferRules
  inferOfflineInput _ = UO.tupleInferRules
  typeInput _ = tupleTypeRules
  termInput _ = tupleTermRules
