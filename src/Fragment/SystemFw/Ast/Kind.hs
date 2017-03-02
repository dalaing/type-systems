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
module Fragment.SystemFw.Ast.Kind (
    KiFSystemFw
  , AsKiSystemFw(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Kind

data KiFSystemFw f =
    KiArrF f f
  deriving (Eq, Ord, Show)

makePrisms ''KiFSystemFw

deriveEq1 ''KiFSystemFw
deriveOrd1 ''KiFSystemFw
deriveShow1 ''KiFSystemFw

class AsKiSystemFw ki where
  _KiSystemFwP :: Prism' (ki j) (KiFSystemFw j)

  _KiArr :: Prism' (Kind ki) (Kind ki, Kind ki)
  _KiArr = _Wrapped . _KiSystemFwP . _KiArrF

instance AsKiSystemFw KiFSystemFw where
  _KiSystemFwP = id

instance {-# OVERLAPPABLE #-} AsKiSystemFw (KiSum xs) => AsKiSystemFw (KiSum (x ': xs)) where
  _KiSystemFwP = _KiNext . _KiSystemFwP

instance {-# OVERLAPPING #-} AsKiSystemFw (KiSum (KiFSystemFw ': xs)) where
  _KiSystemFwP = _KiNow . _KiSystemFwP
