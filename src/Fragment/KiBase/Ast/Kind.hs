{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.KiBase.Ast.Kind (
    KiFBase
  , AsKiBase(..)
  ) where

import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Kind

data KiFBase f =
    KiBaseF
  deriving (Eq, Ord, Show)

makePrisms ''KiFBase

deriveEq1 ''KiFBase
deriveOrd1 ''KiFBase
deriveShow1 ''KiFBase

class AsKiBase ki where
  _KiBaseP :: Prism' (ki j) (KiFBase j)

  _KiBase :: Prism' (Kind ki) ()
  _KiBase = _Wrapped . _KiBaseP . _KiBaseF

instance AsKiBase KiFBase where
  _KiBaseP = id

instance {-# OVERLAPPABLE #-} AsKiBase (KiSum xs) => AsKiBase (KiSum (x ': xs)) where
  _KiBaseP = _KiNext . _KiBaseP

instance {-# OVERLAPPING #-} AsKiBase (KiSum (KiFBase ': xs)) where
  _KiBaseP = _KiNow . _KiBaseP
