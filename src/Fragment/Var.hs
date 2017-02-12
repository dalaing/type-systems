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
{-# LANGUAGE OverloadedStrings #-}
module Fragment.Var (
    AsTmVar(..)
  , HasTmVarSupply(..)
  , ToTmVar(..)
  , freshTmVar
  , tmVar
  , TermContext(..)
  , emptyTermContext
  , HasTermContext(..)
  , AsUnboundTermVariable(..)
  , lookupTerm
  , insertTerm
  , varFragment
  ) where

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens

import Fragment

-- Possibly _TmVar :: Prism' tm a, so we can use it with
-- Term (ASTar a) and pull an a out
class AsTmVar tm where
  _TmVar :: Prism' (tm a) a

-- State

class HasTmVarSupply s where
  tmVarSupply :: Lens' s Int

class ToTmVar a where
  toTmVar :: Int -> a

instance ToTmVar T.Text where
  toTmVar x = T.append "x" (T.pack . show $ x)

freshTmVar :: (MonadState s m, HasTmVarSupply s, ToTmVar a) => m a
freshTmVar = do
  x <- use tmVarSupply
  tmVarSupply %= succ
  return $ toTmVar x

-- Context

data TermContext ty tmV tyV = TermContext (M.Map tmV (ty tyV))

emptyTermContext :: TermContext ty tmV tyV
emptyTermContext = TermContext M.empty

instance HasTermContext (TermContext ty tmV tyV) ty tmV tyV where
  termContext = id

class HasTermContext l ty tmV tyV | l -> ty, l -> tmV, l -> tyV where
  termContext :: Lens' l (TermContext ty tmV tyV)

class AsUnboundTermVariable e tm | e -> tm where
  _UnboundTermVariable :: Prism' e tm

lookupTerm :: (Ord tmV, MonadReader r m, MonadError e m, HasTermContext r ty tmV tyV, AsUnboundTermVariable e tmV) => tmV -> m (ty tyV)
lookupTerm v = do
  TermContext m <- view termContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTermVariable v
    Just ty -> return ty

insertTerm :: Ord tmV => tmV -> ty tyV -> TermContext ty tmV tyV -> TermContext ty tmV tyV
insertTerm v ty (TermContext m) = TermContext (M.insert v ty m)

-- Rules

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, AsTmVar tm, HasTermContext r ty a a, AsUnboundTermVariable e a) => tm a -> Maybe (m (ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

varFragment :: (Ord a, MonadReader r m, HasTermContext r ty a a, MonadError e m, AsUnboundTermVariable e a, AsTmVar tm)
            => FragmentInput e s r m ty p tm a
varFragment =
  FragmentInput
    [] [] [InferBase inferTmVar] [] []

-- Helpers

tmVar :: AsTmVar tm => a -> tm a
tmVar = review _TmVar
