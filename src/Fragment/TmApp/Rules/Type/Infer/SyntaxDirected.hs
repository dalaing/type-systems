{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.TmApp.Rules.Type.Infer.SyntaxDirected (
    TmAppInferTypeContext
  , tmAppInferTypeRules
  ) where

import Control.Monad.Except (MonadError)

import Ast.Error.Common
import Data.Functor.Rec
import Rules.Type.Infer.SyntaxDirected

import Fragment.TyArr.Ast.Error

import qualified Fragment.TmApp.Rules.Type.Infer.Common as A

type TmAppInferTypeContext e w s r m ki ty pt tm a =
  ( A.TmAppInferTypeContext e w s r m m ki ty pt tm a
  , Eq a
  , EqRec (ty ki)
  , MonadError e m
  , AsExpectedTyArr e ki ty a
  , AsExpectedTypeEq e ki ty a
  )

tmAppInferTypeRules :: TmAppInferTypeContext e w s r m ki ty pt tm a
                    => InferTypeInput e w s r m m ki ty pt tm a
tmAppInferTypeRules =
  let
    ah = A.TmAppHelper expectTyArr expectTypeEq
  in
    A.inferTypeInput ah
