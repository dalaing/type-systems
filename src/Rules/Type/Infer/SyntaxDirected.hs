{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Rules.Type.Infer.SyntaxDirected (
    InferTypeRule(..)
  , PCheckRule(..)
  , InferTypeInput(..)
  , InferTypeOutput(..)
  , prepareInferType
  , ISyntax
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

import Data.Functor.Rec

import Ast.Kind
import Ast.Type
import Ast.Term
import Ast.Error.Common

import Rules.Kind.Infer.SyntaxDirected

import Rules.Type.Infer.Common as X

data ISyntax

instance MkInferType ISyntax where
  type MkInferTypeConstraint e w s r m ki ty a ISyntax =
    ( Eq a
    , EqRec (ty ki)
    , MonadError e m
    , AsUnknownTypeError e
    , AsUnexpectedType e ki ty a
    , AsExpectedTypeEq e ki ty a
    , AsExpectedTypeAllEq e ki ty a
    )
  type InferTypeMonad ki ty a m ISyntax =
    m
  type MkInferErrorList ki ty pt tm a ISyntax =
    '[]
  type MkInferWarningList ki ty pt tm a ISyntax =
    '[]

  mkCheckType m i =
    mkCheckType' (expectType m i)

  expectType _ _ e@(ExpectedType ty1) a@(ActualType ty2) =
    unless (ty1 == ty2) $
      throwing _UnexpectedType (e, a)

  expectTypeEq _ _ ty1 ty2 =
    unless (ty1 == ty2) $
      throwing _ExpectedTypeEq (ty1, ty2)

  expectTypeAllEq _ _ n@(ty :| tys) = do
    unless (all (== ty) tys) $
      throwing _ExpectedTypeAllEq (ty :| tys)
    return ty

  prepareInferType pm pi inferKindFn normalizeFn ii =
    let
      u = const . return $ M.empty
      i = mkInferType inferKindFn normalizeFn pc . iiInferTypeRules $ ii
      c = mkCheckType pm pi i
      pc = mkPCheck . iiPCheckRules $ ii
    in
      InferTypeOutput u i c
