{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.SystemFw.Helpers (
    tyLam
  , tyLamAnn
  , tyLamNoAnn
  , tyApp
  ) where

import Control.Lens (review)

import Ast.Kind
import Ast.Type

import Fragment.SystemFw.Ast.Type

tyLam :: (Eq a, AsTySystemFw ki ty) => a -> Maybe (Kind ki a) -> Type ki ty a -> Type ki ty a
tyLam v ki ty = review _TyLam (ki, abstractTy v ty)

tyLamAnn :: (Eq a, AsTySystemFw ki ty) => a -> Kind ki a -> Type ki ty a -> Type ki ty a
tyLamAnn v ki ty = review _TyLamAnn (ki, abstractTy v ty)

tyLamNoAnn :: (Eq a, AsTySystemFw ki ty) => a -> Type ki ty a -> Type ki ty a
tyLamNoAnn v ty = review _TyLamNoAnn (abstractTy v ty)

tyApp :: AsTySystemFw ki ty => Type ki ty a -> Type ki ty a -> Type ki ty a
tyApp = curry $ review _TyApp
