{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmVar.Rules.Type.Infer.Offline (
    TmVarInferTypeContext
  , tmVarInferTypeRules
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)

import Rules.Type.Infer.Offline
import Context.Term

import Fragment.TmVar.Rules.Type.Infer.Common

type TmVarInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, Ord a, MonadReader r m, HasTermContext r ki ty a, MonadError e m, AsUnboundTermVariable e a)

tmVarInferTypeRules :: TmVarInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
tmVarInferTypeRules =
  inferTypeInput
