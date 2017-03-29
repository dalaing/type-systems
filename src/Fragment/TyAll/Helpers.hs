{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.TyAll.Helpers (
    tyAll
  , tyAllAnn
  , tyAllNoAnn
  ) where

import Bound (abstract1)
import Control.Lens (review)

import Ast.Kind
import Ast.Type

import Fragment.TyAll.Ast.Type

tyAll :: (Eq a, AsTyAll ki ty) => a -> Maybe (Kind ki) -> Type ki ty a -> Type ki ty a
tyAll v ki ty = review _TyAll (ki, abstract1 v ty)

tyAllAnn :: (Eq a, AsTyAll ki ty) => a -> Kind ki -> Type ki ty a -> Type ki ty a
tyAllAnn v ki ty = review _TyAllAnn (ki, abstract1 v ty)

tyAllNoAnn :: (Eq a, AsTyAll ki ty) => a -> Type ki ty a -> Type ki ty a
tyAllNoAnn v ty = review _TyAllNoAnn (abstract1 v ty)
