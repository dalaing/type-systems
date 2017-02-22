{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.SystemF.Rules (
    SystemFContext
  , systemFRules
  ) where

import Rules

import Fragment.SystemF.Rules.Infer
import Fragment.SystemF.Rules.Eval

type SystemFContext e s r m ty pt tm a = (SystemFInferContext e s r m ty pt tm a, SystemFEvalContext ty pt tm a)

systemFRules :: SystemFContext e s r m ty pt tm a
          => RulesInput e s r m ty pt tm a
systemFRules =
  RulesInput systemFInferRules systemFEvalRulesLazy systemFEvalRulesStrict
