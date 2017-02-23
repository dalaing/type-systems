{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.PtVar.Rules.Infer (
    PtVarInferContext
  , ptVarInferRules
  ) where

import Control.Lens (preview)

import Rules.Infer

import Ast.Pattern
import Ast.Type

checkVar :: Monad m => Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
checkVar p ty = do
  _ <- preview _PtVar p
  return $
    return [ty]

type PtVarInferContext e s r m ty pt tm a = InferContext e s r m ty pt tm a

ptVarInferRules :: PtVarInferContext e s r m ty pt tm a
                => InferInput e s r m ty pt tm a
ptVarInferRules =
  InferInput
    []
    [ PCheckBase checkVar ]
