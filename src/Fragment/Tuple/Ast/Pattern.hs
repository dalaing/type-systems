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
module Fragment.Tuple.Ast.Pattern (
    PtFTuple
  , AsPtTuple(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Pattern
import Data.Bitransversable
import Data.Functor.Rec

data PtFTuple f a =
  PtTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFTuple

deriveEq1 ''PtFTuple
deriveOrd1 ''PtFTuple
deriveShow1 ''PtFTuple

instance EqRec PtFTuple where
  liftEqRec eR _ (PtTupleF xs) (PtTupleF ys) = and $ zipWith eR xs ys

instance OrdRec PtFTuple where
  liftCompareRec _ _ (PtTupleF []) (PtTupleF []) = EQ
  liftCompareRec _ _ (PtTupleF []) (PtTupleF (_ : _)) = LT
  liftCompareRec _ _ (PtTupleF (_ : _)) (PtTupleF []) = GT
  liftCompareRec cR c (PtTupleF (x : xs)) (PtTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (PtTupleF xs) (PtTupleF ys)
      z -> z

instance ShowRec PtFTuple where
  liftShowsPrecRec _ slR _ _ n (PtTupleF xs) =
    showsUnaryWith (const slR) "PtPupleF" n xs

instance Bound PtFTuple where
  PtTupleF pts >>>= f = PtTupleF (fmap (>>= f) pts)

instance Bitransversable PtFTuple where
  bitransverse fT fL (PtTupleF pts) = PtTupleF <$> traverse (fT fL) pts

class AsPtTuple pt where
  _PtTupleP :: Prism' (pt k a) (PtFTuple k a)

  _PtTuple :: Prism' (Pattern pt a) [Pattern pt a]
  _PtTuple = _PtTree . _PtTupleP . _PtTupleF

instance AsPtTuple PtFTuple where
  _PtTupleP = id

instance {-# OVERLAPPABLE #-} AsPtTuple (PtSum xs) => AsPtTuple (PtSum (x ': xs)) where
  _PtTupleP = _PtNext . _PtTupleP

instance {-# OVERLAPPING #-} AsPtTuple (PtSum (PtFTuple ': xs)) where
  _PtTupleP = _PtNow . _PtTupleP
