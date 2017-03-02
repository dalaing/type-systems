{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.SystemFw.Helpers (
    tyArr
  , tyAll
  , tyLam
  , tyApp
  , tmLam
  , tmApp
  , tmLamTy
  , tmAppTy
  ) where

import Bound (abstract1)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Kind
import Ast.Type
import Ast.Term

import Fragment.SystemFw.Ast.Type
import Fragment.SystemFw.Ast.Term

tyArr :: AsTySystemFw ki ty => Type ki ty a -> Type ki ty a -> Type ki ty a
tyArr = curry $ review _TyArr

tyAll :: (Eq a, AsTySystemFw ki ty) => a -> Kind ki -> Type ki ty a -> Type ki ty a
tyAll v ki ty = review _TyAll (ki, abstract1 v ty)

tyLam :: (Eq a, AsTySystemFw ki ty) => a -> Kind ki -> Type ki ty a -> Type ki ty a
tyLam v ki ty = review _TyLam (ki, abstract1 v ty)

tyApp :: AsTySystemFw ki ty => Type ki ty a -> Type ki ty a -> Type ki ty a
tyApp = curry $ review _TyApp

tmLam :: (Eq a, AsTmSystemFw ki ty pt tm) => a -> Type ki ty a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLam v ty tm = review _TmLam (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmApp :: AsTmSystemFw ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmApp = curry $ review _TmApp

tmLamTy :: (Eq a, AsTmSystemFw ki ty pt tm) => a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLamTy v tm = review _TmLamTy (abstract1 (review _ATyVar v) . review _Unwrapped $ tm)

tmAppTy :: AsTmSystemFw ki ty pt tm => Term ki ty pt tm a -> Type ki ty a -> Term ki ty pt tm a
tmAppTy = curry $ review _TmAppTy
