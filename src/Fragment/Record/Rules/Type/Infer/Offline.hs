{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Record.Rules.Type.Infer.Offline (
    RecordInferTypeContext
  , recordInferTypeRules
  ) where

import Rules.Type.Infer.Offline

import qualified Fragment.Record.Rules.Type.Infer.Common as R

type RecordInferTypeContext e w s r m ki ty pt tm a =
  R.RecordInferTypeContext e w s r m (UnifyT ki ty a m) ki ty pt tm a

recordInferTypeRules :: RecordInferTypeContext e w s r m ki ty pt tm a
                     => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
recordInferTypeRules =
  R.inferTypeInput
