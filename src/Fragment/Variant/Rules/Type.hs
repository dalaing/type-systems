{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Variant.Rules.Type (
    VariantNormalizeConstraint
  , variantNormalizeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Variant.Ast.Type

type VariantNormalizeConstraint ki ty a = AsTyVariant ki ty

normalizeVariant :: VariantNormalizeConstraint ki ty a
               => (Type ki ty a -> Type ki ty a)
               -> Type ki ty a
               -> Maybe (Type ki ty a)
normalizeVariant normalizeFn ty = do
  tys <- preview _TyVariant ty
  return $ review _TyVariant (fmap (fmap normalizeFn) tys)

variantNormalizeRules :: VariantNormalizeConstraint ki ty a
                      => NormalizeInput ki ty a
variantNormalizeRules =
  NormalizeInput [ NormalizeTypeRecurse normalizeVariant ]
