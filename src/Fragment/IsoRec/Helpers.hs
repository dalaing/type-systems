{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.IsoRec.Helpers (
    tyRec
  , tmFold
  , tmUnfold
  ) where

import Bound (Bound)
import Control.Lens (review)

import Ast.Type
import Ast.Term
import Data.Bitransversable

import Fragment.IsoRec.Ast.Type
import Fragment.IsoRec.Ast.Term

tyRec :: (Eq a, Bound ki, Bitransversable ki, AsTyIsoRec ki ty)
      => a
      -> Type ki ty a
      -> Type ki ty a
tyRec v ty = review _TyRec (abstractTy v ty)

tmFold :: AsTmIsoRec ki ty pt tm => Type ki ty a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmFold = curry $ review _TmFold

tmUnfold :: AsTmIsoRec ki ty pt tm => Type ki ty a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmUnfold = curry $ review _TmUnfold
