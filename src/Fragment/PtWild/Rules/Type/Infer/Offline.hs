{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.PtWild.Rules.Type.Infer.Offline (
    PtWildInferTypeContext
  , ptWildInferTypeRules
  ) where

import Ast.Pattern
import Rules.Type.Infer.Offline

import Fragment.PtWild.Rules.Type.Infer.Common

type PtWildInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsPtWild pt)

ptWildInferTypeRules :: PtWildInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
ptWildInferTypeRules =
  inferTypeInput
