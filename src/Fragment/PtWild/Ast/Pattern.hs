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
module Fragment.PtWild.Ast.Pattern (
    PtFWild
  , AsPtWild(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Pattern
import Util

data PtFWild (f :: * -> *) a =
    PtWildF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFWild

deriveEq1 ''PtFWild
deriveOrd1 ''PtFWild
deriveShow1 ''PtFWild

instance EqRec PtFWild where
  liftEqRec _ _ PtWildF PtWildF = True

instance OrdRec PtFWild where
  liftCompareRec _ _ PtWildF PtWildF = EQ

instance ShowRec PtFWild where
  liftShowsPrecRec _ _ _ _ n PtWildF = showsPrec n PtWildF

instance Bound PtFWild where
  PtWildF >>>= _ = PtWildF

instance Bitransversable PtFWild where
  bitransverse _ _ PtWildF = pure PtWildF

class AsPtWild pt where
  _PtWildP :: Prism' (pt k a) (PtFWild k a)

  _PtWild :: Prism' (Pattern pt a) ()
  _PtWild = _PtTree . _PtWildP . _PtWildF

instance AsPtWild PtFWild where
  _PtWildP = id

instance {-# OVERLAPPABLE #-} AsPtWild (PtSum xs) => AsPtWild (PtSum (x ': xs)) where
  _PtWildP = _PtNext . _PtWildP

instance {-# OVERLAPPING #-} AsPtWild (PtSum (PtFWild ': xs)) where
  _PtWildP = _PtNow . _PtWildP
