{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules.Type (
    TupleNormalizeConstraint
  , tupleNormalizeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Tuple.Ast.Type

type TupleNormalizeConstraint ki ty a = AsTyTuple ki ty

normalizeTuple :: TupleNormalizeConstraint ki ty a
               => (Type ki ty a -> Type ki ty a)
               -> Type ki ty a
               -> Maybe (Type ki ty a)
normalizeTuple normalizeFn ty = do
  tys <- preview _TyTuple ty
  return $ review _TyTuple (fmap normalizeFn tys)

tupleNormalizeRules :: TupleNormalizeConstraint ki ty a
                    => NormalizeInput ki ty a
tupleNormalizeRules =
  NormalizeInput [ NormalizeTypeRecurse normalizeTuple ]
