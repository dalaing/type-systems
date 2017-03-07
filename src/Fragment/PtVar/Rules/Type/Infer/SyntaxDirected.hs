{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules.Type.Infer.SyntaxDirected (
    PtVarInferTypeContext
  , ptVarInferTypeRules
  ) where

import Control.Lens (preview)

import Rules.Type.Infer.SyntaxDirected

import Ast.Pattern
import Ast.Type

checkVar :: Monad m => Pattern pt a -> Type ki ty a -> Maybe (m [Type ki ty a])
checkVar p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

type PtVarInferTypeContext e w s r m ki ty pt tm a = InferTypeContext e w s r m ki ty pt tm a

ptVarInferTypeRules :: PtVarInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m ki ty pt tm a
ptVarInferTypeRules =
  InferTypeInput
    []
    [ PCheckBase checkVar ]
