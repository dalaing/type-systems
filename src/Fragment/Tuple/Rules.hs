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
  type InferKindContextSyntax e w s r m ki ty a RTuple = KSD.TupleInferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a RTuple = TSD.TupleInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RTuple = TUO.TupleInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RTuple = TupleTypeContext ki ty a
  type RuleTermContext ki ty pt tm a RTuple = TupleTermContext ki ty pt tm a
  type KindList RTuple = '[KiFBase]
  type TypeList RTuple = '[TyFTuple]
  type ErrorList ki ty pt tm a RTuple = '[ErrExpectedTyTuple ki ty a, ErrTupleOutOfBounds]
  type WarningList ki ty pt tm a RTuple = '[]
  type PatternList RTuple = '[PtFTuple]
  type TermList RTuple = '[TmFTuple]

  inferKindInputSyntax _ = KSD.tupleInferKindRules
  inferTypeInputSyntax _ = TSD.tupleInferTypeRules
  inferTypeInputOffline _ = TUO.tupleInferTypeRules
  typeInput _ = tupleTypeRules
  termInput _ = tupleTermRules
