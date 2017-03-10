{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.PtVar.Rules.Type.Infer.Common (
    inferTypeRules
  ) where

import Control.Lens (preview)

import Ast.Pattern
import Ast.Type

import Rules.Type.Infer.Common

checkVar :: Monad m => Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkVar p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

inferTypeRules :: Monad mi
                => InferTypeInput e w s r m mi ki ty pt tm a
inferTypeRules =
  InferTypeInput [] [] [ PCheckBase checkVar ]

