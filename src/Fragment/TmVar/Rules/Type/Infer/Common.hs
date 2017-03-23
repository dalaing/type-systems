{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Fragment.TmVar.Rules.Type.Infer.Common (
    TmVarInferTypeConstraint
  , tmVarInferTypeInput
  ) where

import Data.Proxy (Proxy)

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Ast.Type
import Ast.Term
import Context.Term

import Rules.Type.Infer.Common

type TmVarInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , Ord a
  , MonadReader r (InferTypeMonad ki ty a m i)
  , HasTermContext r ki ty a
  , MonadError e (InferTypeMonad ki ty a m i)
  , AsUnboundTermVariable e a
  )

inferTmVar :: TmVarInferTypeConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmVar _ _ tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

tmVarInferTypeInput :: TmVarInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
tmVarInferTypeInput m i =
  InferTypeInput [] [ InferTypeBase $ inferTmVar m i ] []
