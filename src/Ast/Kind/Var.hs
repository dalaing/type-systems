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
module Ast.Kind.Var (
    HasKiVarSupply(..)
  , ToKiVar(..)
  , freshKiVar
  ) where

import Control.Monad.State (MonadState)
import Control.Lens (Lens', use, (%=))

import qualified Data.Text as T

class HasKiVarSupply s where
  kiVarSupply :: Lens' s Int

instance HasKiVarSupply Int where
  kiVarSupply = id

class ToKiVar a where
  toKiVar :: Int -> a

instance ToKiVar String where
  toKiVar x = 'k' : show x

instance ToKiVar T.Text where
  toKiVar x = T.append "k" (T.pack . show $ x)

freshKiVar :: (MonadState s m, HasKiVarSupply s, ToKiVar a) => m a
freshKiVar = do
  x <- use kiVarSupply
  kiVarSupply %= succ
  return $ toKiVar x
