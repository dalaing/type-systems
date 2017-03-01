{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Int.Helpers (
    tyInt
  , ptInt
  , tmInt
  , tmAdd
  , tmMul
  ) where

import Control.Lens (review)

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Int.Ast.Type
import Fragment.Int.Ast.Pattern
import Fragment.Int.Ast.Term

tyInt :: AsTyInt ki ty => Type ki ty a
tyInt = review _TyInt ()

ptInt :: AsPtInt pt => Int -> Pattern pt a
ptInt = review _PtInt

tmInt :: AsTmInt ki ty pt tm => Int -> Term ki ty pt tm a
tmInt = review _TmInt

tmAdd :: AsTmInt ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmAdd = curry $ review _TmAdd

tmMul :: AsTmInt ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmMul = curry $ review _TmMul
