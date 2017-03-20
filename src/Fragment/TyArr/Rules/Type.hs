{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TyArr.Rules.Type (
    TyArrNormalizeConstraint
  , tyArrNormalizeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.TyArr.Ast.Type

type TyArrNormalizeConstraint ki ty a = AsTyArr ki ty

normalizeArr :: TyArrNormalizeConstraint ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeArr normalizeFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ review _TyArr (normalizeFn ty1, normalizeFn ty2)

tyArrNormalizeRules :: TyArrNormalizeConstraint ki ty a
                    => NormalizeInput ki ty a
tyArrNormalizeRules =
  NormalizeInput [ NormalizeTypeRecurse normalizeArr ]
