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
    tmLetRec
  , tmLet
  , checkLetBindings
  ) where

import Data.List (elemIndex)

import Bound (abstract)
import Bound.Scope (bindings)
import Control.Lens (review)
import Control.Lens.Wrapped (_Unwrapped)

import Ast.Type
import Ast.Term

import Fragment.Let.Ast.Term

tmLetRec :: (Eq a, TmAstBound ki ty pt tm, AsTmLet ki ty pt tm)
         => [(a, Maybe (Type ki ty a), Term ki ty pt tm a)]
         -> Term ki ty pt tm a
         -> Term ki ty pt tm a
tmLetRec bs tm =
  let
    bs' =
      fmap f bs
    f (v, ty, tm') =
      (TmAstTmVar v, ty, abstr . review _Unwrapped $ tm')
    abstr =
      abstract (`elemIndex` fmap (\(x, _, _) -> x) bs')
    ast =
      abstr $ review _Unwrapped tm
  in
    review _TmLet (fmap (\(_, ty, s) -> LetBinding (fmap (review _TmType) ty) s) bs', ast)

tmLet :: (Eq a, TmAstBound ki ty pt tm, AsTmLet ki ty pt tm)
      => [(a, Maybe (Type ki ty a), Term ki ty pt tm a)]
      -> Term ki ty pt tm a
      -> Term ki ty pt tm a
tmLet bs tm =
  let
    bs' =
      zipWith f [0..] bs
    f i (v, ty, tm') =
      (TmAstTmVar v, ty, abstr i . review _Unwrapped $ tm')
    trim i j
      | i < j = Just j
      | otherwise = Nothing
    abstr i =
      abstract (\v -> (>>= trim i) . elemIndex v . fmap (\(x, _, _) -> x) $ bs')
    ast =
      abstr (length bs') $ review _Unwrapped tm
  in
    review _TmLet (fmap (\(_, ty, s) -> LetBinding (fmap (review _TmType) ty) s) bs', ast)

checkLetBindings :: Foldable tm => [LetBinding ki ty pt tm a] -> Bool
checkLetBindings bs =
  let
    f i (LetBinding _ ast) = all (< i) . bindings $ ast
  in
    and . zipWith f [0..] $ bs

