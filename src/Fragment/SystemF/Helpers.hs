{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.SystemF.Helpers (
    tyArr
  , tyAll
  , tmLam
  , tmApp
  , tmLamTy
  , tmAppTy
  ) where

import Bound (abstract1)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Type
import Ast.Term

import Fragment.SystemF.Ast.Type
import Fragment.SystemF.Ast.Term

tyArr :: AsTySystemF ty => Type ty a -> Type ty a -> Type ty a
tyArr = curry $ review _TyArr

tyAll :: (Eq a, AsTySystemF ty) => a -> Type ty a -> Type ty a
tyAll v ty = review _TyAll (abstract1 v ty)

tmLam :: (Eq a, AsTmSystemF ty pt tm) => a -> Type ty a -> Term ty pt tm a -> Term ty pt tm a
tmLam v ty tm = review _TmLam (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmApp :: AsTmSystemF ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmApp = curry $ review _TmApp

tmLamTy :: (Eq a, AsTmSystemF ty pt tm) => a -> Term ty pt tm a -> Term ty pt tm a
tmLamTy v tm = review _TmLamTy (abstract1 (review _ATyVar v) . review _Unwrapped $ tm)

tmAppTy :: AsTmSystemF ty pt tm => Term ty pt tm a -> Type ty a -> Term ty pt tm a
tmAppTy = curry $ review _TmAppTy
