{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules (
    RecordContext
  , recordRules
  ) where

import Rules

import Fragment.Record.Rules.Infer
import Fragment.Record.Rules.Eval

type RecordContext e s r m ty pt tm a = (RecordInferContext e s r m ty pt tm a, RecordEvalContext ty pt tm a)

recordRules :: RecordContext e s r m ty pt tm a
            => RulesInput e s r m ty pt tm a
recordRules =
  RulesInput recordInferRules recordEvalRulesLazy recordEvalRulesStrict
