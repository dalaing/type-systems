{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Fragment.Annotation (
    module X
  , AnnotationTag
  ) where

import GHC.Exts (Constraint)

import Ast
import Rules.Type
import Rules.Type.Infer.Common
import Rules.Term

import Fragment.Annotation.Ast as X
import Fragment.Annotation.Helpers as X

import Fragment.Annotation.Rules.Type.Infer.Common
import Fragment.Annotation.Rules.Term

data AnnotationTag

instance AstIn AnnotationTag where
  type KindList AnnotationTag = '[]
  type TypeList AnnotationTag = '[]
  type TypeSchemeList AnnotationTag = '[]
  type PatternList AnnotationTag = '[]
  type TermList AnnotationTag = '[TmFAnnotation]

instance EvalRules e AnnotationTag where
  type EvalConstraint ki ty pt tm a e AnnotationTag =
    AnnotationEvalConstraint ki ty pt tm a

  evalInput _ _ =
    annotationEvalRules

instance NormalizeRules AnnotationTag where
  type NormalizeConstraint ki ty a AnnotationTag =
    (() :: Constraint)

  normalizeInput _ =
    mempty

instance MkInferType i => InferTypeRules i AnnotationTag where
  type InferTypeConstraint e w s r m ki ty pt tm a i AnnotationTag =
    AnnotationInferTypeConstraint e w s r m ki ty pt tm a i
  type InferTypeErrorList ki ty pt tm a i AnnotationTag =
    '[]
  type InferTypeWarningList ki ty pt tm a i AnnotationTag =
    '[]

  inferTypeInput m i _ =
    annotationInferTypeInput m i
