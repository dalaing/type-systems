{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.STLC.Helpers (
    tyArr
  , tmLam
  , tmApp
  ) where

import Bound (abstract1)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Type
import Ast.Term

import Fragment.STLC.Ast.Type
import Fragment.STLC.Ast.Term

tyArr :: AsTySTLC ty => Type ty a -> Type ty a -> Type ty a
tyArr = curry $ review _TyArr

tmLam :: (Eq a, AsTmSTLC ty pt tm) => a -> Type ty a -> Term ty pt tm a -> Term ty pt tm a
tmLam v ty tm = review _TmLam (ty, abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmApp :: AsTmSTLC ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmApp = curry $ review _TmApp
