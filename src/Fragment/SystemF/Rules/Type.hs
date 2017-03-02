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
    SystemFTypeContext
  , systemFTypeRules
  ) where

import Bound (toScope, fromScope)
import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.SystemF.Ast.Type

type SystemFTypeContext ki ty a = AsTySystemF ki ty

normalizeArr :: SystemFTypeContext ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeArr normalizeFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ review _TyArr (normalizeFn ty1, normalizeFn ty2)

normalizeAll :: SystemFTypeContext ki ty a
             => (forall b. Type ki ty b -> Type ki ty b)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeAll normalizeFn ty = do
  s <- preview _TyAll ty
  return $ review _TyAll (toScope . normalizeFn . fromScope $ s)

systemFTypeRules :: SystemFTypeContext ki ty a
              => TypeInput ki ty a
systemFTypeRules =
  TypeInput
    [ NormalizeTypeRecurse normalizeArr
    , NormalizeTypeRecurse normalizeAll
    ]
