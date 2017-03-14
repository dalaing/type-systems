{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Annotation.Rules.Term (
    AnnotationTermContext
  , annotationTermRules
  ) where

import Control.Lens (review, preview)

import Rules.Term
import Ast.Term

import Fragment.Annotation.Ast.Term

type AnnotationTermContext ki ty pt tm a = AsTmAnnotation ki ty pt tm

stepAnnotation :: AsTmAnnotation ki ty pt tm
               => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a))
               -> Term ki ty pt tm a
               -> Maybe (Term ki ty pt tm a)
stepAnnotation stepFn tm = do
  (tyA, tmA) <- preview _TmAnnotation tm
  tmA' <- stepFn tmA
  return $ review _TmAnnotation (tyA, tmA')

annotationEvalRules :: AnnotationTermContext ki ty pt tm a
                    => EvalInput ki ty pt tm a
annotationEvalRules =
  EvalInput
    [] [ EvalStep stepAnnotation ] []

annotationTermRules :: AnnotationTermContext ki ty pt tm a
                    => TermInput ki ty pt tm a
annotationTermRules =
  TermInput annotationEvalRules annotationEvalRules
