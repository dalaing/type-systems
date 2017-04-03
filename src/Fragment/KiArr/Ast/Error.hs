{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.KiArr.Ast.Error (
    ErrExpectedKiArr(..)
  , AsExpectedKiArr(..)
  , expectKiArr
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Kind
import Ast.Error

import Fragment.KiArr.Ast.Kind

data ErrExpectedKiArr ki a = ErrExpectedKiArr (Kind ki a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedKiArr

class AsExpectedKiArr e ki a where -- | e -> ty, e -> a where
  _ExpectedKiArr :: Prism' e (Kind ki a)

instance AsExpectedKiArr (ErrExpectedKiArr ki a) ki a where
  _ExpectedKiArr = _ErrExpectedKiArr

instance {-# OVERLAPPABLE #-} AsExpectedKiArr (ErrSum xs) ki a => AsExpectedKiArr (ErrSum (x ': xs)) ki a where
  _ExpectedKiArr = _ErrNext . _ExpectedKiArr

instance {-# OVERLAPPING #-} AsExpectedKiArr (ErrSum (ErrExpectedKiArr ki a ': xs)) ki a where
  _ExpectedKiArr = _ErrNow . _ExpectedKiArr

expectKiArr :: (MonadError e m, AsKiArr ki, AsExpectedKiArr e ki a) => Kind ki a -> m (Kind ki a, Kind ki a)
expectKiArr ty =
  case preview _KiArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedKiArr ty
