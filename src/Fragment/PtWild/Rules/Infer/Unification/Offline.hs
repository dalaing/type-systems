{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules.Infer.Unification.Offline (
    PtWildInferContext
  , ptWildInferRules
  ) where

import Control.Lens (preview)

import Rules.Infer.Unification.Offline

import Ast.Pattern
import Ast.Type

checkWild :: (Monad m, AsPtWild pt) => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkWild p _ = do
  _ <- preview _PtWild p
  return $
    return []

type PtWildInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, AsPtWild pt)

ptWildInferRules :: PtWildInferContext e w s r m ty pt tm a
                => InferInput e w s r m ty pt tm a
ptWildInferRules =
  InferInput
    []
    []
    []
    [ PCheckBase checkWild ]
