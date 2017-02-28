{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.HM.Helpers (
    tyArr
  , tmLam
  , tmApp
  ) where

import Bound (abstract1)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Type
import Ast.Term

import Fragment.HM.Ast.Type
import Fragment.HM.Ast.Term

tyArr :: AsTyHM ty => Type ty a -> Type ty a -> Type ty a
tyArr = curry $ review _TyArr

tmLam :: (Eq a, AsTmHM ty pt tm) => a -> Term ty pt tm a -> Term ty pt tm a
tmLam v tm = review _TmLam (abstract1 (review _ATmVar v) . review _Unwrapped $ tm)

tmApp :: AsTmHM ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmApp = curry $ review _TmApp
