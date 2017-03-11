{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TyArr.Rules.Type (
    TyArrTypeContext
  , tyArrTypeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.TyArr.Ast.Type

type TyArrTypeContext ki ty a = AsTyArr ki ty

normalizeArr :: TyArrTypeContext ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeArr normalizeFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ review _TyArr (normalizeFn ty1, normalizeFn ty2)

tyArrTypeRules :: TyArrTypeContext ki ty a
              => TypeInput ki ty a
tyArrTypeRules =
  TypeInput [ NormalizeTypeRecurse normalizeArr ]
