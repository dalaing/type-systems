{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.TmLam.Helpers (
    tmLam
  , tmLamAnn
  , tmLamNoAnn
  ) where

import Bound (abstract1)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Type
import Ast.Term

import Fragment.TmLam.Ast.Term

tmLam :: (Eq a, AsTmLam ki ty pt tm) => a -> Maybe (Type ki ty a) -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLam v ty tm = review _TmLam (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmLamAnn :: (Eq a, AsTmLam ki ty pt tm) => a -> Type ki ty a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLamAnn v ty tm = review _TmLamAnn (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmLamNoAnn :: (Eq a, AsTmLam ki ty pt tm) => a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmLamNoAnn v tm = review _TmLamNoAnn (abstract1 (review _ATmVar v) . review _Unwrapped $ tm)
