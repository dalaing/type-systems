{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.If.Rules.Type.Infer.SyntaxDirected (
    IfInferTypeContext
  , ifInferTypeRules
  ) where

import Rules.Type.Infer.SyntaxDirected
import Ast.Error.Common

import Fragment.Bool.Ast.Type
import Fragment.If.Ast.Term

import Fragment.If.Rules.Type.Infer.Common

type IfInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, AsUnexpectedType e ki ty a, AsExpectedTypeEq e ki ty a, AsTyBool ki ty, AsTmIf ki ty pt tm)

ifInferTypeRules :: IfInferTypeContext e w s r m ki ty pt tm a
                 => InferTypeInput e w s r m m ki ty pt tm a
ifInferTypeRules =
  let
    ih = IfHelper expectType expectTypeEq
  in
    inferTypeInput ih
