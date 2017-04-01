{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Annotation.Rules.Type.Infer.Common (
    AnnotationInferTypeConstraint
  , annotationInferTypeInput
  ) where

import Data.Proxy (Proxy)

import Control.Lens (preview)

import Ast.Type
import Ast.Error.Common
import Ast.Term
import Rules.Type.Infer.Common

import Fragment.Annotation.Ast.Term

type AnnotationInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsTmAnnotation ki ty pt tm
  )

annotationInferTypeInput :: AnnotationInferTypeConstraint e w s r m ki ty pt tm a i
                         => Proxy (MonadProxy e w s r m)
                         -> Proxy i
                         -> InferTypeInput e w s r m (InferTypeMonad m ki ty a i) ki ty pt tm a
annotationInferTypeInput m i =
  InferTypeInput [] [InferTypeRecurse $ inferTmAnnotation m i ] []

inferTmAnnotation :: AnnotationInferTypeConstraint e w s r m ki ty pt tm a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
            -> Term ki ty pt tm a
            -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmAnnotation m i inferFn tm = do
  (tyE, tmAnn) <- preview _TmAnnotation tm
  return $ do
    tyA <- inferFn tmAnn
    expectType m i (ExpectedType tyE) (ActualType tyA)
    return tyE
