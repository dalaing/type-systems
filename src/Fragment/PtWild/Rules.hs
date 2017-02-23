{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules (
    PtWildContext
  , ptWildRules
  ) where

import Rules

import Fragment.PtWild.Rules.Infer
import Fragment.PtWild.Rules.Eval

type PtWildContext e s r m ty pt tm a = (PtWildInferContext e s r m ty pt tm a, PtWildEvalContext ty pt tm a)

ptWildRules :: PtWildContext e s r m ty pt tm a
         => RulesInput e s r m ty pt tm a
ptWildRules =
  RulesInput ptWildInferRules ptWildEvalRules ptWildEvalRules
