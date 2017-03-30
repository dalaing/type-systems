{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Fragment.SystemF.Rules.Type (
    SystemFNormalizeConstraint
  , systemFNormalizeRules
  ) where

import Bound (Bound)
import Control.Lens (review, preview)

import Ast.Type
import Data.Bitransversable
import Rules.Type

import Fragment.SystemF.Ast.Type

type SystemFNormalizeConstraint ki ty a =
  ( AsTySystemF ki ty
  , Bound ki
  , Bitransversable ki
  )

normalizeArr :: SystemFNormalizeConstraint ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeArr normalizeFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ review _TyArr (normalizeFn ty1, normalizeFn ty2)

normalizeAll :: SystemFNormalizeConstraint ki ty a
             => (forall b. Type ki ty b -> Type ki ty b)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeAll normalizeFn ty = do
  s <- preview _TyAll ty
  return $ review _TyAll (scopeAppTy normalizeFn s)

systemFNormalizeRules :: SystemFNormalizeConstraint ki ty a
                      => NormalizeInput ki ty a
systemFNormalizeRules =
  NormalizeInput
    [ NormalizeTypeRecurse normalizeArr
    , NormalizeTypeRecurse normalizeAll
    ]
