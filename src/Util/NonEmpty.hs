{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Util.NonEmpty (
    NE(..)
  ) where

import Control.Lens.TH (makeWrapped)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Deriving (makeLiftEq, makeLiftCompare, makeLiftShowsPrec)

newtype NE a = NE { unNE :: NonEmpty a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeWrapped ''NE

instance Eq1 NE where
  liftEq e (NE x) (NE y) = $(makeLiftEq ''NonEmpty) e x y

instance Ord1 NE where
  liftCompare c (NE x) (NE y) = $(makeLiftCompare ''NonEmpty) c x y

instance Show1 NE where
  liftShowsPrec s sl n (NE x) = $(makeLiftShowsPrec ''NonEmpty) s sl n x
