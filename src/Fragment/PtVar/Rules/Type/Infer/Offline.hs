{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules.Type.Infer.Offline (
    PtVarInferContext
  , ptVarInferRules
  ) where

import Control.Lens (preview)

import Rules.Type.Infer.Offline

import Ast.Pattern
import Ast.Type

checkVar :: Monad m => Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkVar p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

type PtVarInferContext e w s r m ki ty pt tm a = InferContext e w s r m ki ty pt tm a

ptVarInferRules :: PtVarInferContext e w s r m ki ty pt tm a
                => InferInput e w s r m ki ty pt tm a
ptVarInferRules =
  InferInput
    []
    []
    [ PCheckBase checkVar ]
