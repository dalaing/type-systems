{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Tuple.Helpers (
    tyTuple
  , ptTuple
  , tmTuple
  , tmTupleIx
  ) where

import Control.Lens (review)

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Tuple.Ast.Type
import Fragment.Tuple.Ast.Pattern
import Fragment.Tuple.Ast.Term

tyTuple :: AsTyTuple ty => [Type ty a] -> Type ty a
tyTuple = review _TyTuple

ptTuple :: AsPtTuple pt => [Pattern pt a] -> Pattern pt a
ptTuple = review _PtTuple

tmTuple :: AsTmTuple ty pt tm => [Term ty pt tm a] -> Term ty pt tm a
tmTuple = review _TmTuple

tmTupleIx :: AsTmTuple ty pt tm => Term ty pt tm a -> Int -> Term ty pt tm a
tmTupleIx = curry $ review _TmTupleIx
