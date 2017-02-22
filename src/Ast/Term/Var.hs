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
module Ast.Term.Var (
    HasTmVarSupply(..)
  , ToTmVar(..)
  , freshTmVar
  ) where

import Control.Monad.State (MonadState)
import Control.Lens (Lens', use, (%=))

import qualified Data.Text as T

class HasTmVarSupply s where
  tmVarSupply :: Lens' s Int

instance HasTmVarSupply Int where
  tmVarSupply = id

class ToTmVar a where
  toTmVar :: Int -> a

instance ToTmVar String where
  toTmVar x = 'x' : show x

instance ToTmVar T.Text where
  toTmVar x = T.append "x" (T.pack . show $ x)

freshTmVar :: (MonadState s m, HasTmVarSupply s, ToTmVar a) => m a
freshTmVar = do
  x <- use tmVarSupply
  tmVarSupply %= succ
  return $ toTmVar x
