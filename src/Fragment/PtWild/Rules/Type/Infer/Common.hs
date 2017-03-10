{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.PtWild.Rules.Type.Infer.Common (
    inferTypeInput
  ) where

import Control.Lens (preview)

import Ast.Pattern
import Ast.Type

import Rules.Type.Infer.Common

checkWild :: (AsPtWild pt, Monad m) => Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkWild p _ = do
  _ <- preview _PtWild p
  return $
    return []

inferTypeInput :: (AsPtWild pt, Monad mi)
                => InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput =
  InferTypeInput [] [] [ PCheckBase checkWild ]
