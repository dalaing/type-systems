{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Annotation.Rules (
    RAnnotation
  ) where

import GHC.Exts (Constraint)

import Rules

import Fragment.Annotation.Ast
import qualified Fragment.Annotation.Rules.Type.Infer.SyntaxDirected as TSD
import qualified Fragment.Annotation.Rules.Type.Infer.Offline as TUO
import Fragment.Annotation.Rules.Term

data RAnnotation

instance AstIn RAnnotation where
  type KindList RAnnotation = '[]
  type TypeList RAnnotation = '[]
  type PatternList RAnnotation = '[]
  type TermList RAnnotation = '[TmFAnnotation]

instance RulesIn RAnnotation where
  type InferKindContextSyntax e w s r m ki ty a RAnnotation =
    (() :: Constraint)
  type InferTypeContextSyntax e w s r m ki ty pt tm a RAnnotation =
    TSD.AnnotationInferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a RAnnotation =
    TUO.AnnotationInferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a RAnnotation =
    (() :: Constraint)
  type RuleTermContext ki ty pt tm a RAnnotation =
    AnnotationTermContext ki ty pt tm a
  type ErrorList ki ty pt tm a RAnnotation = '[]
  type WarningList ki ty pt tm a RAnnotation = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = TSD.annotationInferTypeRules
  inferTypeInputOffline _ = TUO.annotationInferTypeRules
  typeInput _ = mempty
  termInput _ = annotationTermRules
