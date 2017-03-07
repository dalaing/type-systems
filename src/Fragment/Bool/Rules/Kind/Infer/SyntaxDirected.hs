{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Bool.Rules.Kind.Infer.SyntaxDirected (
    BoolInferKindContext
  , boolInferKindRules
  ) where

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.Bool.Ast.Type

inferTyBool :: (Monad m, AsKiBase ki, AsTyBool ki ty)
            => Type ki ty a
            -> Maybe (m (Kind ki))
inferTyBool ty = do
  _ <- preview _TyBool ty
  return . return . review _KiBase $ ()

type BoolInferKindContext e w s r m ki ty a = (Monad m, AsKiBase ki, AsTyBool ki ty)

boolInferKindRules :: BoolInferKindContext e w s r m ki ty a
                   => InferKindInput e w s r m ki ty a
boolInferKindRules =
  InferKindInput
    [InferKindBase inferTyBool]
