{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Annotation.Rules.Type.Infer.Offline (
    AnnotationInferTypeContext
  , annotationInferTypeRules
  ) where

import Rules.Type.Infer.Offline

import Fragment.Annotation.Ast.Term
import Fragment.Annotation.Rules.Type.Infer.Common

type AnnotationInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsTmAnnotation ki ty pt tm)

annotationInferTypeRules :: AnnotationInferTypeContext e w s r m ki ty pt tm a
                         => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
annotationInferTypeRules =
  let
    ah = AnnotationHelper expectType
  in
    inferTypeInput ah
