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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.Int.Ast.Pattern (
    PtFInt
  , AsPtInt(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Pattern
import Util

data PtFInt (f :: * -> *) a =
  PtIntF Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFInt

deriveEq1 ''PtFInt
deriveOrd1 ''PtFInt
deriveShow1 ''PtFInt

instance EqRec PtFInt where
  liftEqRec _ _ (PtIntF i) (PtIntF j) = i == j

instance OrdRec PtFInt where
  liftCompareRec _ _ (PtIntF i) (PtIntF j) = compare i j

instance ShowRec PtFInt where
  liftShowsPrecRec _ _ _ _ = showsPrec

instance Bound PtFInt where
  PtIntF i >>>= _ = PtIntF i

instance Bitransversable PtFInt where
  bitransverse _ _ (PtIntF i) = pure $ PtIntF i

class AsPtInt pt where
  _PtIntP :: Prism' (pt k a) (PtFInt k a)

  _PtInt :: Prism' (Pattern pt a) Int
  _PtInt = _PtTree . _PtIntP . _PtIntF

instance AsPtInt PtFInt where
  _PtIntP = id

instance {-# OVERLAPPABLE #-} AsPtInt (PtSum xs) => AsPtInt (PtSum (x ': xs)) where
  _PtIntP = _PtNext . _PtIntP

instance {-# OVERLAPPING #-} AsPtInt (PtSum (PtFInt ': xs)) where
  _PtIntP = _PtNow . _PtIntP
