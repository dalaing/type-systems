{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Record.Helpers (
    tyRecord
  , ptRecord
  , tmRecord
  , tmRecordIx
  ) where

import Control.Lens (review)

import qualified Data.Text as T

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Record.Ast.Type
import Fragment.Record.Ast.Pattern
import Fragment.Record.Ast.Term

tyRecord :: AsTyRecord ty => [(T.Text, Type ty a)] -> Type ty a
tyRecord = review _TyRecord

ptRecord :: AsPtRecord pt => [(T.Text, Pattern pt a)] -> Pattern pt a
ptRecord = review _PtRecord

tmRecord :: AsTmRecord ty pt tm => [(T.Text, Term ty pt tm a)] -> Term ty pt tm a
tmRecord = review _TmRecord

tmRecordIx :: AsTmRecord ty pt tm => Term ty pt tm a -> T.Text -> Term ty pt tm a
tmRecordIx = curry $ review _TmRecordIx
