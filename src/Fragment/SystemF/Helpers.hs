{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.SystemF.Helpers (
    tmLamTy
  , tmLamTyAnn
  , tmLamTyNoAnn
  , tmAppTy
  ) where

import Bound (abstract1)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Kind
import Ast.Type
import Ast.Term

import Fragment.SystemF.Ast.Term

tmLamTy :: (Eq a, AsTmSystemF ki ty pt tm) => a -> Maybe (Kind ki a) -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLamTy v ki tm = review _TmLamTy (ki, abstract1 (review _TmAstTyVar v) . review _Unwrapped $ tm)

tmLamTyAnn :: (Eq a, AsTmSystemF ki ty pt tm) => a -> Kind ki a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLamTyAnn v ki tm = review _TmLamTyAnn (ki, abstract1 (review _TmAstTyVar v) . review _Unwrapped $ tm)

tmLamTyNoAnn :: (Eq a, AsTmSystemF ki ty pt tm) => a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLamTyNoAnn v tm = review _TmLamTyNoAnn (abstract1 (review _TmAstTyVar v) . review _Unwrapped $ tm)

tmAppTy :: AsTmSystemF ki ty pt tm => Term ki ty pt tm a -> Type ki ty a -> Term ki ty pt tm a
tmAppTy = curry $ review _TmAppTy
