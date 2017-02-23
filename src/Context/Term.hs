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
  , lookupTerm
  , insertTerm
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (Lens', view)

import Control.Monad.Error.Lens (throwing)

import qualified Data.Map as M

import Ast.Type

import Context.Term.Error

data TermContext ty a = TermContext (M.Map a (Type ty a))

emptyTermContext :: TermContext ty a
emptyTermContext = TermContext M.empty

class HasTermContext l ty a | l -> ty, l -> a where
  termContext :: Lens' l (TermContext ty a)

instance HasTermContext (TermContext ty a) ty a where
  termContext = id

lookupTerm :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ty a, AsUnboundTermVariable e a) => a -> m (Type ty a)
lookupTerm v = do
  TermContext m <- view termContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTermVariable v
    Just ty -> return ty

insertTerm :: Ord a => a -> Type ty a -> TermContext ty a -> TermContext ty a
insertTerm v ty (TermContext m) = TermContext (M.insert v ty m)
