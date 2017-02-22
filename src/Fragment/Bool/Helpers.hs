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

tyBool :: AsTyBool ty => Type ty a
tyBool = review _TyBool ()

ptBool :: AsPtBool pt => Bool -> Pattern pt a
ptBool = review _PtBool

tmBool :: AsTmBool ty pt tm => Bool -> Term ty pt tm a
tmBool = review _TmBool

tmAnd :: AsTmBool ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmAnd = curry $ review _TmAnd

tmOr :: AsTmBool ty pt tm => Term ty pt tm a -> Term ty pt tm a -> Term ty pt tm a
tmOr = curry $ review _TmOr
