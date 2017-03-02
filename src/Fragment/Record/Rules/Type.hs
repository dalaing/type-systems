{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Type (
    RecordTypeContext
  , recordTypeRules
  ) where

import Data.List (sortOn)

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Record.Ast.Type

type RecordTypeContext ki ty a = AsTyRecord ki ty

normalizeRecord :: RecordTypeContext ki ty a
               => (Type ki ty a -> Type ki ty a)
               -> Type ki ty a
               -> Maybe (Type ki ty a)
normalizeRecord normalizeFn ty = do
  tys <- preview _TyRecord ty
  return $ review _TyRecord (sortOn fst . fmap (fmap normalizeFn) $ tys)

recordTypeRules :: RecordTypeContext ki ty a
              => TypeInput ki ty a
recordTypeRules =
  TypeInput [ NormalizeTypeRecurse normalizeRecord ]
