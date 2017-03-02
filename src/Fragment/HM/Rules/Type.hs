{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.HM.Rules.Type (
    HMTypeContext
  , hmTypeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.HM.Ast.Type

type HMTypeContext ki ty a = AsTyHM ki ty

normalizeArr :: HMTypeContext ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeArr normalizeFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ review _TyArr (normalizeFn ty1, normalizeFn ty2)

hmTypeRules :: HMTypeContext ki ty a
              => TypeInput ki ty a
hmTypeRules =
  TypeInput [ NormalizeTypeRecurse normalizeArr ]
