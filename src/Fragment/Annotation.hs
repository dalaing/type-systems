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

import Ast
import Rules.Term

import Fragment.Annotation.Ast as X
import Fragment.Annotation.Rules as X
import Fragment.Annotation.Helpers as X

import Fragment.Annotation.Rules.Term

data AnnotationTag

instance AstIn AnnotationTag where
  type KindList AnnotationTag = '[]
  type TypeList AnnotationTag = '[]
  type PatternList AnnotationTag = '[]
  type TermList AnnotationTag = '[TmFAnnotation]

instance EvalRules e AnnotationTag where
  type EvalConstraint ki ty pt tm a e AnnotationTag =
    AnnotationEvalConstraint ki ty pt tm a

  evalInput _ _ =
    annotationEvalRules
