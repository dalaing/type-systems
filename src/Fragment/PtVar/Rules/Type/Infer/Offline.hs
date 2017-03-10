{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.PtVar.Rules.Type.Infer.Offline (
    PtVarInferTypeContext
  , ptVarInferTypeRules
  ) where

import Rules.Type.Infer.Offline
import Fragment.PtVar.Rules.Type.Infer.Common

type PtVarInferTypeContext e w s r m ki ty pt tm a = InferTypeContext e w s r m ki ty pt tm a

ptVarInferTypeRules :: PtVarInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
ptVarInferTypeRules =
  inferTypeRules
