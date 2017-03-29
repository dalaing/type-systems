{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Context.Term (
    TermContext(..)
  , emptyTermContext
  , HasTermContext'(..)
  , HasTermContext
  , AsUnboundTermVariable(..)
  , lookupTermBindings
  , lookupTerm
  , insertTerm
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (Lens', view)

import Control.Monad.Error.Lens (throwing)

import qualified Data.Map as M
import qualified Data.Set as S

import Ast.Type

import Context.Term.Error

data TermContext ki ty a = TermContext (M.Map a (Type ki ty a))

emptyTermContext :: TermContext ki ty a
emptyTermContext = TermContext M.empty

class HasTermContext' l where
  type TmCtxKi l :: ((* -> *) -> * -> *)
  type TmCtxTy l :: (((* -> *) -> * -> *) -> (* -> *) -> * -> *)
  type TmCtxVar l :: *

  termContext :: Lens' l (TermContext (TmCtxKi l) (TmCtxTy l) (TmCtxVar l))

instance HasTermContext' (TermContext ki ty a) where
  type TmCtxKi (TermContext ki ty a) = ki
  type TmCtxTy (TermContext ki ty a) = ty
  type TmCtxVar (TermContext ki ty a) = a

  termContext = id

type HasTermContext r ki ty a = (HasTermContext' r, ki ~ TmCtxKi r, ty ~ TmCtxTy r, a ~ TmCtxVar r)

lookupTermBindings :: (MonadReader r m, HasTermContext r ki ty a) => m (S.Set a)
lookupTermBindings = do
  TermContext m <- view termContext
  return $ M.keysSet m

lookupTerm :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ki ty a, AsUnboundTermVariable e a) => a -> m (Type ki ty a)
lookupTerm v = do
  TermContext m <- view termContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTermVariable v
    Just ty -> return ty

insertTerm :: Ord a => a -> Type ki ty a -> TermContext ki ty a -> TermContext ki ty a
insertTerm v ty (TermContext m) = TermContext (M.insert v ty m)
