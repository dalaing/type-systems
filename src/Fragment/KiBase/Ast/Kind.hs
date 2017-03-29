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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.KiBase.Ast.Kind (
    KiFBase
  , AsKiBase(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Kind
import Data.Bitransversable
import Data.Functor.Rec

data KiFBase (f :: * -> *) a =
    KiBaseF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''KiFBase

deriveEq1 ''KiFBase
deriveOrd1 ''KiFBase
deriveShow1 ''KiFBase

instance EqRec KiFBase where
  liftEqRec _ _ KiBaseF KiBaseF =
    True

instance OrdRec KiFBase where
  liftCompareRec _ _ KiBaseF KiBaseF =
    EQ

instance ShowRec KiFBase where
  liftShowsPrecRec _ _ _ _ _ KiBaseF =
    showString "KiBaseF"

instance Bound KiFBase where
  KiBaseF >>>= _ = KiBaseF

instance Bitransversable KiFBase where
  bitransverse _ _ KiBaseF = pure KiBaseF

class AsKiBase ki where
  _KiBaseP :: Prism' (ki j a) (KiFBase j a)

  _KiBase :: Prism' (Kind ki a) ()
  _KiBase = _KiTree . _KiBaseP . _KiBaseF

instance AsKiBase KiFBase where
  _KiBaseP = id

instance {-# OVERLAPPABLE #-} AsKiBase (KiSum xs) => AsKiBase (KiSum (x ': xs)) where
  _KiBaseP = _KiNext . _KiBaseP

instance {-# OVERLAPPING #-} AsKiBase (KiSum (KiFBase ': xs)) where
  _KiBaseP = _KiNow . _KiBaseP
