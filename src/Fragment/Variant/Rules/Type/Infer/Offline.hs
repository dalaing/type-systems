{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Variant.Rules.Type.Infer.Offline (
    VariantInferTypeContext
  , variantInferTypeRules
  ) where

import Rules.Type.Infer.Offline

import qualified Fragment.Variant.Rules.Type.Infer.Common as V

type VariantInferTypeContext e w s r m ki ty pt tm a =
  V.VariantInferTypeContext e w s r m (UnifyT ki ty a m) ki ty pt tm a

variantInferTypeRules :: VariantInferTypeContext e w s r m ki ty pt tm a
                      => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
variantInferTypeRules =
  let
    vh = V.VariantHelper expectTypeEq
  in
    V.inferTypeInput vh
