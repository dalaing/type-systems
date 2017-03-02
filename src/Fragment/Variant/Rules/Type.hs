{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules.Type (
    VariantTypeContext
  , variantTypeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Variant.Ast.Type

type VariantTypeContext ki ty a = AsTyVariant ki ty

normalizeVariant :: VariantTypeContext ki ty a
               => (Type ki ty a -> Type ki ty a)
               -> Type ki ty a
               -> Maybe (Type ki ty a)
normalizeVariant normalizeFn ty = do
  tys <- preview _TyVariant ty
  return $ review _TyVariant (fmap (fmap normalizeFn) tys)

variantTypeRules :: VariantTypeContext ki ty a
              => TypeInput ki ty a
variantTypeRules =
  TypeInput [ NormalizeTypeRecurse normalizeVariant ]
