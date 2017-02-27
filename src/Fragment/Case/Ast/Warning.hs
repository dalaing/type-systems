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
module Fragment.Case.Ast.Warning (
    WarnUnusedPatternVariables(..)
  , AsUnusedPatternVariables(..)
  , checkForUnusedPatternVariables
  , WarnShadowingPatternVariables(..)
  , AsShadowingPatternVariables(..)
  , checkForShadowingPatternVariables
  ) where

import Data.List ((\\))

import Control.Monad.Writer (MonadWriter(..))
import Control.Lens (review)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import qualified Data.List.NonEmpty as N
import qualified Data.Set as S

import Ast.Warning

data WarnUnusedPatternVariables a = WarnUnusedPatternVariables (N.NonEmpty a)
  deriving (Eq, Ord, Show)

makePrisms ''WarnUnusedPatternVariables

class AsUnusedPatternVariables e a where -- | e -> a where
  _UnusedPatternVariables :: Prism' e (N.NonEmpty a)

instance AsUnusedPatternVariables (WarnUnusedPatternVariables a) a where
  _UnusedPatternVariables = _WarnUnusedPatternVariables

instance {-# OVERLAPPABLE #-} AsUnusedPatternVariables (WarnSum xs) a => AsUnusedPatternVariables (WarnSum (x ': xs)) a where
  _UnusedPatternVariables = _WarnNext . _UnusedPatternVariables

instance {-# OVERLAPPING #-} AsUnusedPatternVariables (WarnSum (WarnUnusedPatternVariables a ': xs)) a where
  _UnusedPatternVariables = _WarnNow . _UnusedPatternVariables

checkForUnusedPatternVariables :: (MonadWriter [w] m, AsUnusedPatternVariables w a) => [a] -> [Int] -> m ()
checkForUnusedPatternVariables vs is =
  let
    n = length vs
    unusedI = [0..(n - 1)] \\ is
    unusedV = map (vs !!) unusedI
  in
    case N.nonEmpty unusedV of
      Nothing -> return ()
      Just ns -> tell (pure . review _UnusedPatternVariables $ ns)

data WarnShadowingPatternVariables a = WarnShadowingPatternVariables (N.NonEmpty a)
  deriving (Eq, Ord, Show)

makePrisms ''WarnShadowingPatternVariables

class AsShadowingPatternVariables e a where -- | e -> a where
  _ShadowingPatternVariables :: Prism' e (N.NonEmpty a)

instance AsShadowingPatternVariables (WarnShadowingPatternVariables a) a where
  _ShadowingPatternVariables = _WarnShadowingPatternVariables

instance {-# OVERLAPPABLE #-} AsShadowingPatternVariables (WarnSum xs) a => AsShadowingPatternVariables (WarnSum (x ': xs)) a where
  _ShadowingPatternVariables = _WarnNext . _ShadowingPatternVariables

instance {-# OVERLAPPING #-} AsShadowingPatternVariables (WarnSum (WarnShadowingPatternVariables a ': xs)) a where
  _ShadowingPatternVariables = _WarnNow . _ShadowingPatternVariables

checkForShadowingPatternVariables :: (Ord a, MonadWriter [w] m, AsShadowingPatternVariables w a) => [a] -> S.Set a -> m ()
checkForShadowingPatternVariables vs cvs =
  let
    shadowing = filter (`S.member` cvs) vs
  in
    case N.nonEmpty shadowing of
      Nothing -> return ()
      Just ns -> tell (pure . review _ShadowingPatternVariables $ ns)
