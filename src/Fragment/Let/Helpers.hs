{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Let.Helpers (
    tmLet
  ) where

import Data.List (elemIndex)

import Bound (abstract)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Type
import Ast.Term

import Fragment.Let.Ast.Term

tmLet :: (Eq a, AstBound ki ty pt tm, AsTmLet ki ty pt tm)
      => [(a, Maybe (Type ki ty a), Term ki ty pt tm a)]
      -> Term ki ty pt tm a
      -> Term ki ty pt tm a
tmLet bindings tm =
  let
    bs = fmap f bindings
    f (v, ty, tm') = (ATmVar v, ty, abstr . review _Unwrapped $ tm')
    abstr = abstract (`elemIndex` map (\(x, _, _) -> x) bs)
  in
    review _TmLet (fmap (\(_, ty, s) -> LetBinding (fmap (review _Type) ty) s) bs, abstr . review _Unwrapped $ tm)


