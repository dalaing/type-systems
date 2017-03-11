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
module Fragment.TmLam.Ast.Error (
    ErrExpectedTmLamAnnotation(..)
  , AsExpectedTmLamAnnotation(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import Ast.Error

data ErrExpectedTmLamAnnotation = ErrExpectedTmLamAnnotation
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedTmLamAnnotation

class AsExpectedTmLamAnnotation e where -- | e -> ty, e -> a where
  _ExpectedTmLamAnnotation :: Prism' e ()

instance AsExpectedTmLamAnnotation ErrExpectedTmLamAnnotation where
  _ExpectedTmLamAnnotation = _ErrExpectedTmLamAnnotation

instance {-# OVERLAPPABLE #-} AsExpectedTmLamAnnotation (ErrSum xs) => AsExpectedTmLamAnnotation (ErrSum (x ': xs)) where
  _ExpectedTmLamAnnotation = _ErrNext . _ExpectedTmLamAnnotation

instance {-# OVERLAPPING #-} AsExpectedTmLamAnnotation (ErrSum (ErrExpectedTmLamAnnotation ': xs)) where
  _ExpectedTmLamAnnotation = _ErrNow . _ExpectedTmLamAnnotation
