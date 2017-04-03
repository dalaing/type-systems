{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.SystemFw.Ast.Error (
    ErrExpectedTyLamAnnotation(..)
  , AsExpectedTyLamAnnotation(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Error

data ErrExpectedTyLamAnnotation = ErrExpectedTyLamAnnotation
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTyLamAnnotation

class AsExpectedTyLamAnnotation e where -- | e -> ty, e -> a where
  _ExpectedTyLamAnnotation :: Prism' e ()

instance AsExpectedTyLamAnnotation ErrExpectedTyLamAnnotation where
  _ExpectedTyLamAnnotation = _ErrExpectedTyLamAnnotation

instance {-# OVERLAPPABLE #-} AsExpectedTyLamAnnotation (ErrSum xs) => AsExpectedTyLamAnnotation (ErrSum (x ': xs)) where
  _ExpectedTyLamAnnotation = _ErrNext . _ExpectedTyLamAnnotation

instance {-# OVERLAPPING #-} AsExpectedTyLamAnnotation (ErrSum (ErrExpectedTyLamAnnotation ': xs)) where
  _ExpectedTyLamAnnotation = _ErrNow . _ExpectedTyLamAnnotation
