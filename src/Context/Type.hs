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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Context.Type (
    TypeContext(..)
  , emptyTypeContext
  , HasTypeContext'(..)
  , HasTypeContext
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

data TypeContext ki a = TypeContext (M.Map a (Kind ki a))

emptyTypeContext :: TypeContext ki a
emptyTypeContext = TypeContext M.empty

class HasTypeContext' l where
  type TyCtxKi l :: ((* -> *) -> * -> *)
  type TyCtxVar l :: *

  typeContext :: Lens' l (TypeContext (TyCtxKi l) (TyCtxVar l))

instance HasTypeContext' (TypeContext ki a)where
  type TyCtxKi (TypeContext ki a) = ki
  type TyCtxVar (TypeContext ki a) = a

  typeContext = id

type HasTypeContext r ki a = (HasTypeContext' r, ki ~ TyCtxKi r, a ~ TyCtxVar r)

lookupTypeBindings :: (MonadReader r m, HasTypeContext r ki a) => m (S.Set a)
lookupTypeBindings = do
  TypeContext m <- view typeContext
  return $ M.keysSet m

lookupType :: (Ord a, MonadReader r m, MonadError e m, HasTypeContext r ki a, AsUnboundTypeVariable e a) => a -> m (Kind ki a)
lookupType v = do
  TypeContext m <- view typeContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTypeVariable v
    Just ty -> return ty

insertType :: Ord a => a -> Kind ki a -> TypeContext ki a -> TypeContext ki a
insertType v ty (TypeContext m) = TypeContext (M.insert v ty m)
