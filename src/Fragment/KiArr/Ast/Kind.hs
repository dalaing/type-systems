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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.KiArr.Ast.Kind (
    KiFArr
  , AsKiArr(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Kind

data KiFArr f =
  KiArrF f f
  deriving (Eq, Ord, Show)

makePrisms ''KiFArr

deriveEq1 ''KiFArr
deriveOrd1 ''KiFArr
deriveShow1 ''KiFArr

class AsKiArr ki where
  _KiArrP :: Prism' (ki j) (KiFArr j)

  _KiArr :: Prism' (Kind ki) (Kind ki, Kind ki)
  _KiArr = _Wrapped . _KiArrP . _KiArrF

instance AsKiArr KiFArr where
  _KiArrP = id

instance {-# OVERLAPPABLE #-} AsKiArr (KiSum xs) => AsKiArr (KiSum (x ': xs)) where
  _KiArrP = _KiNext . _KiArrP

instance {-# OVERLAPPING #-} AsKiArr (KiSum (KiFArr ': xs)) where
  _KiArrP = _KiNow . _KiArrP
