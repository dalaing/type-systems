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
module Fragment.Record.Ast.Error (
    ErrExpectedTyRecord(..)
  , AsExpectedTyRecord(..)
  , expectTyRecord
  , ErrRecordNotFound(..)
  , AsRecordNotFound(..)
  , lookupRecord
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import qualified Data.Text as T

import Ast.Type
import Ast.Error

import Fragment.Record.Ast.Type

data ErrExpectedTyRecord (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrExpectedTyRecord (Type ty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyRecord

class AsExpectedTyRecord e ty a | e -> ty, e -> a where
  _ExpectedTyRecord :: Prism' e (Type ty a)

instance AsExpectedTyRecord (ErrExpectedTyRecord ty pt tm a) ty a where
  _ExpectedTyRecord = _ErrExpectedTyRecord

instance {-# OVERLAPPABLE #-} AsExpectedTyRecord ((ErrSum xs) ty pt tm a) ty a => AsExpectedTyRecord (ErrSum (x ': xs) ty pt tm a) ty a where
  _ExpectedTyRecord = _ErrNext . _ExpectedTyRecord

instance {-# OVERLAPPING #-} AsExpectedTyRecord (ErrSum (ErrExpectedTyRecord ': xs) ty pt tm a) ty a where
  _ExpectedTyRecord = _ErrNow . _ExpectedTyRecord

expectTyRecord :: (MonadError e m, AsExpectedTyRecord e ty a, AsTyRecord ty) => Type ty a -> m [(T.Text, Type ty a)]
expectTyRecord ty =
  case preview _TyRecord ty of
    Just tys -> return tys
    _ -> throwing _ExpectedTyRecord ty

data ErrRecordNotFound (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *) a = ErrRecordNotFound T.Text
  deriving (Eq, Ord, Show)

makePrisms ''ErrRecordNotFound

class AsRecordNotFound e where
  _RecordNotFound :: Prism' e T.Text

instance AsRecordNotFound (ErrRecordNotFound ty pt tm a) where
  _RecordNotFound = _ErrRecordNotFound

instance {-# OVERLAPPABLE #-} AsRecordNotFound ((ErrSum xs) ty pt tm a) => AsRecordNotFound (ErrSum (x ': xs) ty pt tm a) where
  _RecordNotFound = _ErrNext . _RecordNotFound

instance {-# OVERLAPPING #-} AsRecordNotFound (ErrSum (ErrRecordNotFound ': xs) ty pt tm a) where
  _RecordNotFound = _ErrNow . _RecordNotFound

lookupRecord :: (MonadError e m, AsRecordNotFound e) =>  [(T.Text, t a)] -> T.Text -> m (t a)
lookupRecord ts t =
  case lookup t ts of
    Just x -> return x
    Nothing -> throwing _RecordNotFound t
