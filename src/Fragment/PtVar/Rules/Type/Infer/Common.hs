{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.PtVar.Rules.Type.Infer.Common (
    PtVarInferTypeConstraint
  , ptVarInferTypeInput
  ) where

import Data.Proxy (Proxy)

import Control.Lens (preview)

import Ast.Pattern
import Ast.Type

import Rules.Type.Infer.Common

type PtVarInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  )

checkVar :: PtVarInferTypeConstraint e w s r m ki ty pt tm a i
         => Proxy (MonadProxy e w s r m )
         -> Proxy i
         -> Pattern pt a
         -> Type ki ty a
         -> Maybe (InferTypeMonad ki ty a m i [Type ki ty a])
checkVar _ _ p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

ptVarInferTypeInput :: PtVarInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
ptVarInferTypeInput m i =
  InferTypeInput [] [] [ PCheckBase $ checkVar m i ]

