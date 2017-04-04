{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fragment.Variant.Rules.Kind.Infer.Common (
    VariantInferKindConstraint
  , variantInferKindInput
  ) where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)

import Ast.Kind
import Ast.Type
import Rules.Kind.Infer.Common

import Fragment.KiBase.Ast.Kind
import Fragment.Variant.Ast.Type

type VariantInferKindConstraint e w s r m ki ty a i =
  ( BasicInferKindConstraint e w s r m ki ty a i
  , AsKiBase ki
  , AsTyVariant ki ty
  )

variantInferKindInput :: VariantInferKindConstraint e w s r m ki ty a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferKindInput e w s r m (InferKindMonad m ki a i) ki ty a
variantInferKindInput m i =
  InferKindInput
    []
    [ InferKindRecurse $ inferTyVariant m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ]

inferTyVariant :: VariantInferKindConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> (Type ki ty a -> InferKindMonad m ki a i (Kind ki a))
            -> Type ki ty a
            -> Maybe (InferKindMonad m ki a i (Kind ki a))
inferTyVariant pm pki pty pa pi inferFn ty = do
  tys <- preview _TyVariant ty
  return $ do
    let kib = review _KiBase ()
    traverse_ (traverse (\tyT -> mkCheckKind pm pki pty pa pi inferFn tyT kib)) tys
    return kib
