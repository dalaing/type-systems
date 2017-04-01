{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.Variant.Rules.Type.Infer.Common (
    VariantInferTypeConstraint
  , variantInferTypeInput
  ) where

import Data.Proxy (Proxy)

import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Ast.Type
import Ast.Pattern
import Ast.Term

import Fragment.Variant.Ast.Type
import Fragment.Variant.Ast.Error
import Fragment.Variant.Ast.Pattern
import Fragment.Variant.Ast.Term

import Rules.Type.Infer.Common

type VariantInferTypeConstraint e w s r m ki ty pt tm a i =
  ( VariantInferConstraint e w s r m ki ty pt tm a i
  , VariantCheckConstraint e w s r m ki ty pt tm a i
  )

type VariantInferConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsTmVariant ki ty pt tm
  , AsTyVariant ki ty
  , MonadError e (InferTypeMonad m ki ty a i)
  , AsExpectedTyVariant e ki ty a
  , AsVariantNotFound e
  )

type VariantCheckConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsPtVariant pt
  , AsTyVariant ki ty
  , MonadError e (InferTypeMonad m ki ty a i)
  , AsExpectedTyVariant e ki ty a
  , AsVariantNotFound e
  )

variantInferTypeInput :: VariantInferTypeConstraint e w s r m ki ty pt tm a i
                      => Proxy (MonadProxy e w s r m)
                      -> Proxy i
                      -> InferTypeInput e w s r m (InferTypeMonad m ki ty a i) ki ty pt tm a
variantInferTypeInput m i =
  InferTypeInput
    []
    [ InferTypeRecurse $ inferTmVariant m i ]
    [ PCheckRecurse $ checkVariant m i ]

inferTmVariant :: VariantInferConstraint e w s r m ki ty pt tm a i
               => Proxy (MonadProxy e w s r m)
               -> Proxy i
               -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
               -> Term ki ty pt tm a
               -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmVariant m i inferFn tm = do
  (l, tmV, ty) <- preview _TmVariant tm
  return $ do
    tyL <- inferFn tmV
    tys <- expectTyVariant ty
    tyV <- lookupVariant tys l
    expectTypeEq m i tyL tyV
    return ty

checkVariant :: VariantCheckConstraint e w s r m ki ty pt tm a i
             => Proxy (MonadProxy e w s r m)
             -> Proxy i
             -> (Pattern pt a -> Type ki ty a -> InferTypeMonad m ki ty a i [Type ki ty a])
             -> Pattern pt a
             -> Type ki ty a
             -> Maybe (InferTypeMonad m ki ty a i [Type ki ty a])
checkVariant _ _ checkFn p ty = do
  (lV, pV) <- preview _PtVariant p
  return $ do
    vs <- expectTyVariant ty
    tyV <- lookupVariant vs lV
    checkFn pV tyV

