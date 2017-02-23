{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules (
    PtVarContext
  , ptVarRules
  ) where

import Rules

import Fragment.PtVar.Rules.Infer
import Fragment.PtVar.Rules.Eval

type PtVarContext e s r m ty pt tm a = (PtVarInferContext e s r m ty pt tm a, PtVarEvalContext ty pt tm a)

ptVarRules :: PtVarContext e s r m ty pt tm a
         => RulesInput e s r m ty pt tm a
ptVarRules =
  RulesInput ptVarInferRules ptVarEvalRules ptVarEvalRules
