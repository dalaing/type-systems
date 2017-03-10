{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Type.Infer.SyntaxDirected (
    IntInferTypeContext
  , intInferTypeRules
  ) where

import Control.Lens (review)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

import Fragment.Int.Rules.Type.Infer.Common

createInt :: (AsTyInt ki ty, Monad m)
          => m (Type ki ty a)
createInt =
  return . review _TyInt $ ()

type IntInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsTyInt ki ty, AsPtInt pt, AsTmInt ki ty pt tm)

intInferTypeRules :: IntInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m m ki ty pt tm a
intInferTypeRules =
  let
    ih = IntHelper createInt expectType
  in
    inferTypeInput ih
