{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.STLC.Rules.Type (
    STLCTypeContext
  , stlcTypeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.STLC.Ast.Type

type STLCTypeContext ki ty a = AsTySTLC ki ty

normalizeArr :: STLCTypeContext ki ty a
             => (Type ki ty a -> Type ki ty a)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeArr normalizeFn ty = do
  (ty1, ty2) <- preview _TyArr ty
  return $ review _TyArr (normalizeFn ty1, normalizeFn ty2)

stlcTypeRules :: STLCTypeContext ki ty a
              => TypeInput ki ty a
stlcTypeRules =
  TypeInput [ NormalizeTypeRecurse normalizeArr ]
