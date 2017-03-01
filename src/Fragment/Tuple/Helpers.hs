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

tyTuple :: AsTyTuple ki ty => [Type ki ty a] -> Type ki ty a
tyTuple = review _TyTuple

ptTuple :: AsPtTuple pt => [Pattern pt a] -> Pattern pt a
ptTuple = review _PtTuple

tmTuple :: AsTmTuple ki ty pt tm => [Term ki ty pt tm a] -> Term ki ty pt tm a
tmTuple = review _TmTuple

tmTupleIx :: AsTmTuple ki ty pt tm => Term ki ty pt tm a -> Int -> Term ki ty pt tm a
tmTupleIx = curry $ review _TmTupleIx
