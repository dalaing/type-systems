{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Fragment.TyAll.Rules.Type (
    TyAllNormalizeConstraint
  , tyAllNormalizeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.TyAll.Ast.Type

type TyAllNormalizeConstraint ki ty a = AsTyAll ki ty

normalizeAll :: TyAllNormalizeConstraint ki ty a
             => (forall b. Type ki ty b -> Type ki ty b)
             -> Type ki ty a
             -> Maybe (Type ki ty a)
normalizeAll normalizeFn ty = do
  (k, s) <- preview _TyAll ty
  return $ review _TyAll (k, scopeAppTy normalizeFn s)

tyAllNormalizeRules :: TyAllNormalizeConstraint ki ty a
                    => NormalizeInput ki ty a
tyAllNormalizeRules =
  NormalizeInput
    [ NormalizeTypeRecurse normalizeAll ]
