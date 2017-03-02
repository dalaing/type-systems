{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Int.Rules.Type (
    IntTypeContext
  , intTypeRules
  ) where

import Control.Lens (preview)

import Rules.Type
import Ast.Type

import Fragment.Int.Ast.Type

type IntTypeContext ki ty a = AsTyInt ki ty

normalizeInt :: IntTypeContext ki ty a
              => Type ki ty a
              -> Maybe (Type ki ty a)
normalizeInt ty = do
  _ <- preview _TyInt ty
  return ty

intTypeRules :: IntTypeContext ki ty a
              => TypeInput ki ty a
intTypeRules =
  TypeInput [ NormalizeTypeBase normalizeInt ]
