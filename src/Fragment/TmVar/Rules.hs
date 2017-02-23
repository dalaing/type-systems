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
module Fragment.TmVar.Rules (
    RTmVar
  ) where

import GHC.Exts (Constraint)

import Rules
import Context.Term.Error

import Fragment.TmVar.Rules.Infer

data RTmVar

instance RulesIn RTmVar where
  type RuleInferContext e s r m ty pt tm a RTmVar = TmVarInferContext e s r m ty pt tm a
  type RuleEvalContext ty tm pt a RTmVar = (() :: Constraint)
  type TypeList RTmVar = '[]
  type ErrorList ty tm pt a RTmVar = '[ErrUnboundTermVariable a]
  type PatternList RTmVar = '[]
  type TermList RTmVar = '[]

  inferInput _ = tmVarInferRules
  evalLazyInput _ = mempty
  evalStrictInput _ = mempty
