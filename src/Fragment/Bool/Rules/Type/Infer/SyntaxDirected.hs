{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules.Type.Infer.SyntaxDirected (
    BoolInferTypeContext
  , boolInferTypeRules
  ) where

import Control.Lens (review)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

import Fragment.Bool.Rules.Type.Infer.Common

createBool :: (AsTyBool ki ty, Monad m)
           => m (Type ki ty a)
createBool =
  return . review _TyBool $ ()

type BoolInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsTyBool ki ty, AsPtBool pt, AsTmBool ki ty pt tm)

boolInferTypeRules :: BoolInferTypeContext e w s r m ki ty pt tm a
                   => InferTypeInput e w s r m m ki ty pt tm a
boolInferTypeRules =
  let
    bh = BoolHelper createBool expectType
  in
    inferTypeInput bh
