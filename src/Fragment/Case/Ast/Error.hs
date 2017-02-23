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
module Fragment.Case.Ast.Error (
    ErrExpectedPattern(..)
  , AsExpectedPattern(..)
  , expectPattern
  , ErrDuplicatedPatternVariables(..)
  , AsDuplicatedPatternVariables(..)
  , checkForDuplicatedPatternVariables
  , ErrUnusedPatternVariables(..)
  , AsUnusedPatternVariables(..)
  , checkForUnusedPatternVariables
  ) where

import Data.List (group, sort, (\\))

import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)
import Control.Lens (preview)
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)

import qualified Data.List.NonEmpty as N

import Ast.Error
import Ast.Pattern
import Ast.Term

data ErrExpectedPattern ty pt tm a = ErrExpectedPattern (Ast ty pt tm (AstVar a))
  deriving (Eq, Ord, Show)

makePrisms ''ErrExpectedPattern

class AsExpectedPattern e ty pt tm a | e -> ty, e -> pt, e -> tm, e -> a where
  _ExpectedPattern :: Prism' e (Ast ty pt tm (AstVar a))

instance AsExpectedPattern (ErrExpectedPattern ty pt tm a) ty pt tm a where
  _ExpectedPattern = _ErrExpectedPattern

instance {-# OVERLAPPABLE #-} AsExpectedPattern ((ErrSum xs) ty pt tm a) ty pt tm a => AsExpectedPattern (ErrSum (x ': xs) ty pt tm a) ty pt tm a where
  _ExpectedPattern = _ErrNext . _ExpectedPattern

instance {-# OVERLAPPING #-} AsExpectedPattern (ErrSum (ErrExpectedPattern ty pt tm a ': xs) ty pt tm a) ty pt tm a where
  _ExpectedPattern = _ErrNow . _ExpectedPattern

expectPattern :: (MonadError e m, AsExpectedPattern e ty pt tm a, AstTransversable ty pt tm) => Ast ty pt tm (AstVar a) -> m (Pattern pt a)
expectPattern ast =
  case preview _Pattern ast of
    Just p -> return p
    _ -> throwing _ExpectedPattern ast

data ErrDuplicatedPatternVariables a = ErrDuplicatedPatternVariables (N.NonEmpty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrDuplicatedPatternVariables

class AsDuplicatedPatternVariables e a | e -> a where
  _DuplicatedPatternVariables :: Prism' e (N.NonEmpty a)

instance AsDuplicatedPatternVariables (ErrDuplicatedPatternVariables a) a where
  _DuplicatedPatternVariables = _ErrDuplicatedPatternVariables

instance {-# OVERLAPPABLE #-} AsDuplicatedPatternVariables ((ErrSum xs) ty pt tm a) a => AsDuplicatedPatternVariables (ErrSum (x ': xs) ty pt tm a) a where
  _DuplicatedPatternVariables = _ErrNext . _DuplicatedPatternVariables

instance {-# OVERLAPPING #-} AsDuplicatedPatternVariables (ErrSum (ErrDuplicatedPatternVariables a ': xs) ty pt tm a) a where
  _DuplicatedPatternVariables = _ErrNow . _DuplicatedPatternVariables

checkForDuplicatedPatternVariables :: (Ord a, MonadError e m, AsDuplicatedPatternVariables e a) => [a] -> m ()
checkForDuplicatedPatternVariables xs =
  let
    dups = map head .
           filter ((> 1) . length) .
           group .
           sort $
           xs
  in
    case N.nonEmpty dups of
      Nothing -> return ()
      Just ns -> throwing _DuplicatedPatternVariables ns

data ErrUnusedPatternVariables a = ErrUnusedPatternVariables (N.NonEmpty a)
  deriving (Eq, Ord, Show)

makePrisms ''ErrUnusedPatternVariables

class AsUnusedPatternVariables e a | e -> a where
  _UnusedPatternVariables :: Prism' e (N.NonEmpty a)

instance AsUnusedPatternVariables (ErrUnusedPatternVariables a) a where
  _UnusedPatternVariables = _ErrUnusedPatternVariables

instance {-# OVERLAPPABLE #-} AsUnusedPatternVariables ((ErrSum xs) ty pt tm a) a => AsUnusedPatternVariables (ErrSum (x ': xs) ty pt tm a) a where
  _UnusedPatternVariables = _ErrNext . _UnusedPatternVariables

instance {-# OVERLAPPING #-} AsUnusedPatternVariables (ErrSum (ErrUnusedPatternVariables a ': xs) ty pt tm a) a where
  _UnusedPatternVariables = _ErrNow . _UnusedPatternVariables

checkForUnusedPatternVariables :: (MonadError e m, AsUnusedPatternVariables e a) => [a] -> [Int] -> m ()
checkForUnusedPatternVariables vs is =
  let
    n = length vs
    unusedI = [0..(n - 1)] \\ is
    unusedV = map (vs !!) unusedI
  in
    case N.nonEmpty unusedV of
      Nothing -> return ()
      Just ns -> throwing _UnusedPatternVariables ns
