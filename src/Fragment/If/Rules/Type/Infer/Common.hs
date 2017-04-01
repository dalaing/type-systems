{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.If.Rules.Type.Infer.Common (
    IfInferTypeConstraint
  , ifInferTypeInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (review, preview)

import Ast.Type
import Ast.Error.Common
import Ast.Term

import Fragment.Bool.Ast.Type
import Fragment.If.Ast.Term

import Rules.Type.Infer.Common

type IfInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsTmIf ki ty pt tm
  , AsTyBool ki ty
  )

ifInferTypeInput :: IfInferTypeConstraint e w s r m ki ty pt tm a i
                 => Proxy (MonadProxy e w s r m)
                 -> Proxy i
                 -> InferTypeInput e w s r m (InferTypeMonad m ki ty a i) ki ty pt tm a
ifInferTypeInput m i =
  InferTypeInput
    [] [ InferTypeRecurse $ inferTmIf m i ] []

inferTmIf :: IfInferTypeConstraint e w s r m ki ty pt tm a i
          => Proxy (MonadProxy e w s r m)
          -> Proxy i
          -> (Term ki ty pt tm a -> InferTypeMonad m ki ty a i (Type ki ty a))
          -> Term ki ty pt tm a
          -> Maybe (InferTypeMonad m ki ty a i (Type ki ty a))
inferTmIf m i inferFn tm = do
  (tmB, tmT, tmF) <- preview _TmIf tm
  return $ do
    tyB <- inferFn tmB
    expectType m i (ExpectedType tyB) (ActualType (review _TyBool ()))
    tyT <- inferFn tmT
    tyF <- inferFn tmF
    expectTypeEq m i tyT tyF
    return tyT
