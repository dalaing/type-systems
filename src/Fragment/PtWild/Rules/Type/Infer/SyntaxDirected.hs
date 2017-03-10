{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules.Type.Infer.SyntaxDirected (
    PtWildInferTypeContext
  , ptWildInferTypeRules
  ) where

import Ast.Pattern
import Rules.Type.Infer.SyntaxDirected

import Fragment.PtWild.Rules.Type.Infer.Common

type PtWildInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsPtWild pt)

ptWildInferTypeRules :: PtWildInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m m ki ty pt tm a
ptWildInferTypeRules =
  inferTypeInput
