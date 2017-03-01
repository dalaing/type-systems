{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Variant.Helpers (
    tyVariant
  , ptVariant
  , tmVariant
  ) where

import Control.Lens (review)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as N

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Variant.Ast.Type
import Fragment.Variant.Ast.Pattern
import Fragment.Variant.Ast.Term

tyVariant :: AsTyVariant ki ty => N.NonEmpty (T.Text, Type ki ty a) -> Type ki ty a
tyVariant = review _TyVariant

ptVariant :: AsPtVariant pt => T.Text -> Pattern pt a -> Pattern pt a
ptVariant = curry $ review _PtVariant

tmVariant :: AsTmVariant ki ty pt tm => T.Text -> Term ki ty pt tm a -> Type ki ty a -> Term ki ty pt tm a
tmVariant l tm ty = review _TmVariant (l, tm, ty)
