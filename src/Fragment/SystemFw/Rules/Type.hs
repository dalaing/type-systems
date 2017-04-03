{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Fragment.SystemFw.Rules.Type (
    SystemFwNormalizeConstraint
  , systemFwNormalizeRules
  ) where

import Bound (toScope, fromScope)
import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.SystemFw.Ast.Type

type SystemFwNormalizeConstraint ki ty a = AsTySystemFw ki ty

normalizeLam :: SystemFwNormalizeConstraint ki ty a
             => (forall b. Type ki ty b -> Type ki ty b)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeLam normalizeFn ty = do
  (k, s) <- preview _TyLam ty
  return $ review _TyLam (k, scopeAppTy normalizeFn s)

normalizeApp :: SystemFwNormalizeConstraint ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeApp normalizeFn ty = do
  (ty1, ty2) <- preview _TyApp ty
  return $ review _TyApp (normalizeFn ty1, normalizeFn ty2)

systemFwNormalizeRules :: SystemFwNormalizeConstraint ki ty a
                       => NormalizeInput ki ty a
systemFwNormalizeRules =
  NormalizeInput
    [ NormalizeTypeRecurse normalizeLam
    , NormalizeTypeRecurse normalizeApp
    ]
