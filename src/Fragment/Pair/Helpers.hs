{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Pair.Helpers (
    tyPair
  , ptPair
  , tmPair
  , tmFst
  , tmSnd
  ) where

import Control.Lens (review)

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Pair.Ast.Type
import Fragment.Pair.Ast.Pattern
import Fragment.Pair.Ast.Term

tyPair :: AsTyPair ki ty => Type ki ty a -> Type ki ty a -> Type ki ty a
tyPair = curry $ review _TyPair

ptPair :: AsPtPair pt => Pattern pt a -> Pattern pt a -> Pattern pt a
ptPair = curry $ review _PtPair

tmPair :: AsTmPair ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmPair = curry $ review _TmPair

tmFst :: AsTmPair ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a
tmFst = review _TmFst

tmSnd :: AsTmPair ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a
tmSnd = review _TmSnd
