{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Record.Rules.Type (
    RecordNormalizeConstraint
  , recordNormalizeRules
  ) where

import Data.List (sortOn)

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Record.Ast.Type

type RecordNormalizeConstraint ki ty a = AsTyRecord ki ty

normalizeRecord :: RecordNormalizeConstraint ki ty a
               => (Type ki ty a -> Type ki ty a)
               -> Type ki ty a
               -> Maybe (Type ki ty a)
normalizeRecord normalizeFn ty = do
  tys <- preview _TyRecord ty
  return $ review _TyRecord (sortOn fst . fmap (fmap normalizeFn) $ tys)

recordNormalizeRules :: RecordNormalizeConstraint ki ty a
                     => NormalizeInput ki ty a
recordNormalizeRules =
  NormalizeInput [ NormalizeTypeRecurse normalizeRecord ]
