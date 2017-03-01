{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Bool.Helpers (
    tyBool
  , ptBool
  , tmBool
  , tmAnd
  , tmOr
  ) where

import Control.Lens (review)

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Bool.Ast.Type
import Fragment.Bool.Ast.Pattern
import Fragment.Bool.Ast.Term

tyBool :: AsTyBool ki ty => Type ki ty a
tyBool = review _TyBool ()

ptBool :: AsPtBool pt => Bool -> Pattern pt a
ptBool = review _PtBool

tmBool :: AsTmBool ki ty pt tm => Bool -> Term ki ty pt tm a
tmBool = review _TmBool

tmAnd :: AsTmBool ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmAnd = curry $ review _TmAnd

tmOr :: AsTmBool ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmOr = curry $ review _TmOr
