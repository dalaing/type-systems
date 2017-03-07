{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules.Kind.Infer.SyntaxDirected (
    VariantInferKindContext
  , variantInferKindRules
  ) where

import Data.Foldable (traverse_)

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Data.Functor.Classes (Eq1)

import Ast.Kind
import Ast.Type
import Ast.Error.Common
import Rules.Kind.Infer.SyntaxDirected

import Fragment.KiBase.Ast.Kind
import Fragment.Variant.Ast.Type

inferTyVariant :: (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyVariant ki ty)
            => (Type ki ty a -> m (Kind ki))
            -> Type ki ty a
            -> Maybe (m (Kind ki))
inferTyVariant inferFn ty = do
  tys <- preview _TyVariant ty
  return $ do
    let ki = review _KiBase()
    traverse_ (\(_, tyV) -> mkCheckKind inferFn tyV ki) tys
    return . review _KiBase $ ()

type VariantInferKindContext e w s r m ki ty a = (MonadError e m, AsUnexpectedKind e ki, Eq1 ki, AsKiBase ki, AsTyVariant ki ty)

variantInferKindRules :: VariantInferKindContext e w s r m ki ty a
                      => InferKindInput e w s r m ki ty a
variantInferKindRules =
  InferKindInput
    [InferKindRecurse inferTyVariant]
