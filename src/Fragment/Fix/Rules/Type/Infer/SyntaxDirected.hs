{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.Fix.Rules.Type.Infer.SyntaxDirected (
    FixInferTypeContext
  , fixInferTypeRules
  ) where

import Control.Monad.Except (MonadError)

import Ast.Error.Common
import Data.Functor.Rec
import Rules.Type.Infer.SyntaxDirected

import Fragment.TyArr.Ast.Error

import qualified Fragment.Fix.Rules.Type.Infer.Common as F

type FixInferTypeContext e w s r m ki ty pt tm a =
  ( F.FixInferTypeContext e w s r m m ki ty pt tm a
  , Eq a
  , EqRec (ty ki)
  , MonadError e m
  , AsExpectedTyArr e ki ty a
  , AsExpectedTypeEq e ki ty a
  )

fixInferTypeRules :: FixInferTypeContext e w s r m ki ty pt tm a
                  => InferTypeInput e w s r m m ki ty pt tm a
fixInferTypeRules =
  let
    fh = F.FixHelper expectTyArr expectTypeEq
  in
    F.inferTypeInput fh
