{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Pair.Rules.Type.Infer.SyntaxDirected (
    PairInferTypeContext
  , pairInferTypeRules
  ) where

import Control.Monad.Except (MonadError)
import Control.Lens (review)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type

import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Error
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

import Fragment.Pair.Rules.Type.Infer.Common

createPair :: (Monad m, AsTyPair ki ty) => Type ki ty a -> Type ki ty a -> m (Type ki ty a)
createPair ty1 ty2 =
  return . review _TyPair $ (ty1, ty2)

expectPair :: (MonadError e m, AsExpectedTyPair e ki ty a, AsTyPair ki ty) => Type ki ty a -> m (Type ki ty a, Type ki ty a)
expectPair =
  expectTyPair

type PairInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsTyPair ki ty, AsExpectedTyPair e ki ty a, AsPtPair pt, AsTmPair ki ty pt tm)

pairInferTypeRules :: PairInferTypeContext e w s r m ki ty pt tm a
              => InferTypeInput e w s r m m ki ty pt tm a
pairInferTypeRules =
  let
    ph = PairHelper createPair expectPair
  in
    inferTypeInput ph
