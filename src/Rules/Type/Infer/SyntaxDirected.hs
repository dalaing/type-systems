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
    ITSyntax
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

import Data.Functor.Rec

import Ast.Error.Common

import Rules.Type.Infer.Common

data ITSyntax

instance MkInferType ITSyntax where
  type MkInferTypeConstraint e w s r m ki ty a ITSyntax =
    ( Eq a
    , EqRec ki
    , EqRec (ty ki)
    , MonadError e m
    , AsUnknownTypeError e
    , AsUnexpectedType e ki ty a
    , AsExpectedTypeEq e ki ty a
    , AsExpectedTypeAllEq e ki ty a
    )
  type InferTypeMonad m ki ty a ITSyntax =
    m
  type MkInferTypeErrorList ki ty pt tm a ITSyntax =
    '[]
  type MkInferTypeWarningList ki ty pt tm a ITSyntax =
    '[]

  mkCheckType m i =
    mkCheckType' (expectType m i)

  expectType _ _ e@(ExpectedType ty1) a@(ActualType ty2) =
    unless (ty1 == ty2) $
      throwing _UnexpectedType (e, a)

  expectTypeEq _ _ ty1 ty2 =
    unless (ty1 == ty2) $
      throwing _ExpectedTypeEq (ty1, ty2)

  expectTypeAllEq _ _ (ty :| tys) = do
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
