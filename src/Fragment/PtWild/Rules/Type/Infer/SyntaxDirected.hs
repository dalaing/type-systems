{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtWild.Rules.Type.Infer.SyntaxDirected (
    PtWildInferTypeContext
  , ptWildInferTypeRules
  ) where

import Control.Lens (preview)

import Rules.Type.Infer.SyntaxDirected

import Ast.Pattern
import Ast.Type

checkWild :: (Monad m, AsPtWild pt) => Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkWild p _ = do
  _ <- preview _PtWild p
  return $
    return []

type PtWildInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsPtWild pt)

ptWildInferTypeRules :: PtWildInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m ki ty pt tm a
ptWildInferTypeRules =
  InferTypeInput
    []
    [ PCheckBase checkWild ]
