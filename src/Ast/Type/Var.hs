{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ast.Type.Var (
    HasTyVarSupply(..)
  , ToTyVar(..)
  , freshTyVar
  ) where

import Control.Monad.State (MonadState)
import Control.Lens (Lens', use, (%=))

import qualified Data.Text as T

class HasTyVarSupply s where
  tyVarSupply :: Lens' s Int

instance HasTyVarSupply Int where
  tyVarSupply = id

class ToTyVar a where
  toTyVar :: Int -> a

instance ToTyVar String where
  toTyVar x = 'x' : show x

instance ToTyVar T.Text where
  toTyVar x = T.append "x" (T.pack . show $ x)

freshTyVar :: (MonadState s m, HasTyVarSupply s, ToTyVar a) => m a
freshTyVar = do
  x <- use tyVarSupply
  tyVarSupply %= succ
  return $ toTyVar x
