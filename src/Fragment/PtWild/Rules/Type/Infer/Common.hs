{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.PtWild.Rules.Type.Infer.Common (
    PtWildInferTypeConstraint
  , ptWildInferTypeInput
  ) where

import Data.Proxy (Proxy)

import Control.Lens (preview)

import Ast.Pattern
import Ast.Type

import Rules.Type.Infer.Common

type PtWildInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsPtWild pt
  )

checkWild :: PtWildInferTypeConstraint e w s r m ki ty pt tm a i
         => Proxy (MonadProxy e w s r m )
         -> Proxy i
         -> Pattern pt a
         -> Type ki ty a
         -> Maybe (InferTypeMonad ki ty a m i [Type ki ty a])
checkWild _ _ p _ = do
  _ <- preview _PtWild p
  return $
    return []

ptWildInferTypeInput :: PtWildInferTypeConstraint e w s r m ki ty pt tm a i
                     => Proxy (MonadProxy e w s r m)
                     -> Proxy i
                     -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
ptWildInferTypeInput m i =
  InferTypeInput [] [] [ PCheckBase $ checkWild m i ]
