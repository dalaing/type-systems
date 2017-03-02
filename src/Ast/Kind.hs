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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Ast.Kind (
    Kind(..)
  , KiSum(..)
  , _KiNow
  , _KiNext
  ) where

import Control.Lens.Prism (Prism', prism)
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makeWrapped, makePrisms)
import Data.Functor.Classes
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

newtype Kind k = Kind (k (Kind k))

makeWrapped ''Kind

instance Eq1 k => Eq (Kind k) where
  Kind x == Kind y = liftEq (==) x y

instance Ord1 k => Ord (Kind k) where
  compare (Kind x) (Kind y) = liftCompare compare x y

instance Show1 k => Show (Kind k) where
  showsPrec n (Kind x) = liftShowsPrec showsPrec showList n x

data KiSum (f :: [k -> *]) (a :: k) where
  KiNext :: KiSum f a -> KiSum (g ': f) a
  KiNow :: g a -> KiSum (g ': f) a

_KiNext :: Prism' (KiSum (f ': g) a) (KiSum g a)
_KiNext = prism KiNext $ \x -> case x of
  KiNext y -> Right y
  _ -> Left x

_KiNow :: Prism' (KiSum (f ': g) a) (f a)
_KiNow = prism KiNow $ \x -> case x of
  KiNow y -> Right y
  _ -> Left x

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
