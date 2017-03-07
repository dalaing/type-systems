{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Kind.Infer.SyntaxDirected (
    IntInferKindContext
  , intInferKindRules
  ) where

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.Int.Ast.Type

inferTyInt :: (Monad m, AsKiBase ki, AsTyInt ki ty)
            => Type ki ty a
            -> Maybe (m (Kind ki))
inferTyInt ty = do
  _ <- preview _TyInt ty
  return . return . review _KiBase $ ()

type IntInferKindContext e w s r m ki ty a = (Monad m, AsKiBase ki, AsTyInt ki ty)

intInferKindRules :: IntInferKindContext e w s r m ki ty a
                  => InferKindInput e w s r m ki ty a
intInferKindRules =
  InferKindInput
    [InferKindBase inferTyInt]
