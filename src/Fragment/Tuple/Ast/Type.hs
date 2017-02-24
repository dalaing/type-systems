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
module Fragment.Tuple.Ast.Type (
    TyFTuple
  , AsTyTuple(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFTuple f a =
  TyTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFTuple

deriveEq1 ''TyFTuple
deriveOrd1 ''TyFTuple
deriveShow1 ''TyFTuple

instance EqRec TyFTuple where
  liftEqRec eR _ (TyTupleF xs) (TyTupleF ys) = and $ zipWith eR xs ys

instance OrdRec TyFTuple where
  liftCompareRec _ _ (TyTupleF []) (TyTupleF []) = EQ
  liftCompareRec _ _ (TyTupleF []) (TyTupleF (_ : _)) = LT
  liftCompareRec _ _ (TyTupleF (_ : _)) (TyTupleF []) = GT
  liftCompareRec cR c (TyTupleF (x : xs)) (TyTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (TyTupleF xs) (TyTupleF ys)
      z -> z

instance ShowRec TyFTuple where
  liftShowsPrecRec _ slR _ _ n (TyTupleF xs) =
    showsUnaryWith (const slR) "TyTupleF" n xs

instance Bound TyFTuple where
  TyTupleF tys >>>= f = TyTupleF (fmap (>>= f) tys)

instance Bitransversable TyFTuple where
  bitransverse fT fL (TyTupleF tys) = TyTupleF <$> traverse (fT fL) tys

class AsTyTuple ty where
  _TyTupleP :: Prism' (ty k a) (TyFTuple k a)

  _TyTuple :: Prism' (Type ty a) [Type ty a]
  _TyTuple = _TyTree . _TyTupleP . _TyTupleF

instance AsTyTuple TyFTuple where
  _TyTupleP = id

instance {-# OVERLAPPABLE #-} AsTyTuple (TySum xs) => AsTyTuple (TySum (x ': xs)) where
  _TyTupleP = _TyNext . _TyTupleP

instance {-# OVERLAPPING #-} AsTyTuple (TySum (TyFTuple ': xs)) where
  _TyTupleP = _TyNow . _TyTupleP
