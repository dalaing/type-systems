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

import Ast
import Rules.Term

import Fragment.Case.Ast as X
import Fragment.Case.Rules as X
import Fragment.Case.Helpers as X

import Fragment.Case.Rules.Term

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
