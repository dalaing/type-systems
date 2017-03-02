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
module Context.Term (
    TermContext(..)
  , emptyTermContext
  , HasTermContext(..)
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

class HasTermContext l ki ty a | l -> ki, l -> ty, l -> a where
  termContext :: Lens' l (TermContext ki ty a)

instance HasTermContext (TermContext ki ty a) ki ty a where
  termContext = id

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
