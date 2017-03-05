{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Context.Type (
    TypeContext(..)
  , emptyTypeContext
  , HasTypeContext(..)
  , AsUnboundTypeVariable(..)
  , lookupTypeBindings
  , lookupType
  , insertType
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (Lens', view)

import Control.Monad.Error.Lens (throwing)

import qualified Data.Map as M
import qualified Data.Set as S

import Ast.Kind

import Context.Type.Error

data TypeContext ki a = TypeContext (M.Map a (Kind ki))

emptyTypeContext :: TypeContext ki a
emptyTypeContext = TypeContext M.empty

class HasTypeContext l ki a | l -> ki, l -> a where
  typeContext :: Lens' l (TypeContext ki a)

instance HasTypeContext (TypeContext ki a) ki a where
  typeContext = id

lookupTypeBindings :: (MonadReader r m, HasTypeContext r ki a) => m (S.Set a)
lookupTypeBindings = do
  TypeContext m <- view typeContext
  return $ M.keysSet m

lookupType :: (Ord a, MonadReader r m, MonadError e m, HasTypeContext r ki a, AsUnboundTypeVariable e a) => a -> m (Kind ki)
lookupType v = do
  TypeContext m <- view typeContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTypeVariable v
    Just ty -> return ty

insertType :: Ord a => a -> Kind ki -> TypeContext ki a -> TypeContext ki a
insertType v ty (TypeContext m) = TypeContext (M.insert v ty m)
