{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Case.Helpers (
    tmAlt
  , tmCase
  ) where

import Data.Foldable (toList)
import Data.List (elemIndex)

import Bound (abstract)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import qualified Data.List.NonEmpty as N

import Ast.Pattern
import Ast.Term

import Fragment.Case.Ast.Term

tmAlt :: (Eq a, AstBound ki ty pt tm, AstTransversable ki ty pt tm) => Pattern pt a -> Term ki ty pt tm a -> Alt ki ty pt (Ast ki ty pt tm) (AstVar a)
tmAlt p tm = Alt (review _Pattern p) s
  where
    vs = fmap ATmVar . toList $ p
    s = abstract (`elemIndex` vs) . review _Unwrapped $ tm

tmCase :: AsTmCase ki ty pt tm => Term ki ty pt tm a -> [Alt ki ty pt (Ast ki ty pt tm) (AstVar a)] -> Term ki ty pt tm a
tmCase tm alts =
  case N.nonEmpty alts of
    Nothing -> tm
    Just xs -> review _TmCase (tm, xs)
