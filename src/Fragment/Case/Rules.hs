{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Case.Rules (
    CaseContext
  , caseRules
  ) where

import Rules

import Fragment.Case.Rules.Infer
import Fragment.Case.Rules.Eval

type CaseContext e s r m ty pt tm a = (CaseInferContext e s r m ty pt tm a, CaseEvalContext ty pt tm a)

caseRules :: CaseContext e s r m ty pt tm a
            => RulesInput e s r m ty pt tm a
caseRules =
  RulesInput caseInferRules caseEvalRulesLazy caseEvalRulesStrict
