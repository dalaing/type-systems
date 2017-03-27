{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Fragment.Case (
    module X
  , CaseTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.Case.Ast as X
import Fragment.Case.Helpers as X

import Fragment.Case.Rules.Term
import Fragment.Case.Rules.Type.Infer.Common

data CaseTag

instance AstIn CaseTag where
  type KindList CaseTag = '[]
  type TypeList CaseTag = '[]
  type PatternList CaseTag = '[]
  type TermList CaseTag = '[TmFCase]

instance EvalRules EStrict CaseTag where
  type EvalConstraint ki ty pt tm a EStrict CaseTag =
    CaseEvalConstraint ki ty pt tm a

  evalInput _ _ =
    caseEvalRulesStrict

instance EvalRules ELazy CaseTag where
  type EvalConstraint ki ty pt tm a ELazy CaseTag =
    CaseEvalConstraint ki ty pt tm a

  evalInput _ _ =
    caseEvalRulesLazy

instance NormalizeRules CaseTag where
  type NormalizeConstraint ki ty a CaseTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i CaseTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i CaseTag =
    CaseInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i CaseTag =
    '[ ErrDuplicatedPatternVariables a
     , ErrExpectedPattern ki ty pt tm a
     ]
  type InferTypeWarningList ki ty pt tm a i CaseTag =
    '[ WarnUnusedPatternVariables a
     , WarnShadowingPatternVariables a
     ]

  inferTypeInput m i _ =
    caseInferTypeInput m i
