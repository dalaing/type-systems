{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Int.Rules.Type.Infer.Offline (
    IOfflineInt
  ) where

import Control.Lens (review)

import Ast.Type
import Ast.Type.Var

import Rules.Type.Infer.Offline (IOffline)
import Control.Monad.State (MonadState)

import Fragment.Int.Rules.Type.Infer.Common

data IOfflineInt

instance IntInferTypeHelper ITOffline IOfflineInt where
  type IntInferTypeHelperConstraint e w s r m ki ty a ITOffline IOfflineInt =
    ( MonadState s m
    , HasTyVarSupply s
    , ToTyVar a
    )

  createInt _ _ _ =
    fmap (review _TyVar) freshTyVar
