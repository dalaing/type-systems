{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Tuple.Rules.Type (
    TupleTypeContext
  , tupleTypeRules
  ) where

import Control.Lens (review, preview)

import Rules.Type
import Ast.Type

import Fragment.Tuple.Ast.Type

type TupleTypeContext ki ty a = AsTyTuple ki ty

normalizeTuple :: TupleTypeContext ki ty a
               => (Type ki ty a -> Type ki ty a)
               -> Type ki ty a
               -> Maybe (Type ki ty a)
normalizeTuple normalizeFn ty = do
  tys <- preview _TyTuple ty
  return $ review _TyTuple (fmap normalizeFn tys)

tupleTypeRules :: TupleTypeContext ki ty a
              => TypeInput ki ty a
tupleTypeRules =
  TypeInput [ NormalizeTypeRecurse normalizeTuple ]
