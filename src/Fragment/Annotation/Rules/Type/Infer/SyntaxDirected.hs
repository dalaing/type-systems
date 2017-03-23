{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Annotation.Rules.Type.Infer.SyntaxDirected (
    AnnotationInferTypeContext
  , annotationInferTypeRules
  ) where

import Rules.Type.Infer.SyntaxDirected

import Fragment.Annotation.Ast.Term
import Fragment.Annotation.Rules.Type.Infer.Common

type AnnotationInferTypeContext e w s r m ki ty pt tm a = AsTmAnnotation ki ty pt tm

annotationInferTypeRules :: AnnotationInferTypeContext e w s r m ki ty pt tm a
                         => InferTypeInput e w s r m m ki ty pt tm a
annotationInferTypeRules =
  let
    ah = AnnotationHelper expectType
  in
    inferTypeInput ah
