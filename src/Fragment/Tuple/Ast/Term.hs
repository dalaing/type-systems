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
module Fragment.Tuple.Ast.Term (
    TmFTuple
  , AsTmTuple(..)
  ) where

import Data.Functor.Classes (showsUnaryWith, showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Iso (mapping, bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

data TmFTuple (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmTupleF [f a]
  | TmTupleIxF (f a) Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFTuple

deriveEq1 ''TmFTuple
deriveOrd1 ''TmFTuple
deriveShow1 ''TmFTuple

instance EqRec (TmFTuple ki ty pt) where
  liftEqRec eR _ (TmTupleF xs) (TmTupleF ys) =
    and $ zipWith eR xs ys
  liftEqRec eR _ (TmTupleIxF x1 i1) (TmTupleIxF x2 i2) =
    eR x1 x2 && i1 == i2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFTuple ki ty pt) where
  liftCompareRec _ _ (TmTupleF []) (TmTupleF []) = EQ
  liftCompareRec _ _ (TmTupleF []) (TmTupleF (_ : _)) = LT
  liftCompareRec _ _ (TmTupleF (_ : _)) (TmTupleF []) = GT
  liftCompareRec cR c (TmTupleF (x : xs)) (TmTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (TmTupleF xs) (TmTupleF ys)
      z -> z
  liftCompareRec _ _ (TmTupleF _) _ = LT
  liftCompareRec _ _ _ (TmTupleF _) = GT
  liftCompareRec cR _ (TmTupleIxF x1 i1) (TmTupleIxF x2 i2) =
    case cR x1 x2 of
      EQ -> compare i1 i2
      z -> z

instance ShowRec (TmFTuple ki ty pt) where
  liftShowsPrecRec _ slR _ _ n (TmTupleF xs) =
    showsUnaryWith (const slR) "TmTupleF" n xs
  liftShowsPrecRec sR _ _ _ n (TmTupleIxF x i) =
    showsBinaryWith sR showsPrec "TmTupleIxF" n x i

instance Bound (TmFTuple ki ty pt) where
  TmTupleF tms >>>= f = TmTupleF (fmap (>>= f) tms)
  TmTupleIxF tm i >>>= f = TmTupleIxF (tm >>= f) i

instance Bitransversable (TmFTuple ki ty pt) where
  bitransverse fT fL (TmTupleF tms) = TmTupleF <$> traverse (fT fL) tms
  bitransverse fT fL (TmTupleIxF tm i) = TmTupleIxF <$> fT fL tm <*> pure i

class AsTmTuple ki ty pt tm where
  _TmTupleP :: Prism' (tm ki ty pt f a) (TmFTuple ki ty pt f a)

  _TmTuple :: Prism' (Term ki ty pt tm a) [Term ki ty pt tm a]
  _TmTuple = _Wrapped . _ATerm . _TmTupleP . _TmTupleF . mapping _Unwrapped

  _TmTupleIx :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Int)
  _TmTupleIx = _Wrapped . _ATerm . _TmTupleP . _TmTupleIxF . bimapping _Unwrapped id

instance AsTmTuple ki ty pt TmFTuple where
  _TmTupleP = id

instance {-# OVERLAPPABLE #-} AsTmTuple ki ty pt (TmSum xs) => AsTmTuple ki ty pt (TmSum (x ': xs)) where
  _TmTupleP = _TmNext . _TmTupleP

instance {-# OVERLAPPING #-} AsTmTuple ki ty pt (TmSum (TmFTuple ': xs)) where
  _TmTupleP = _TmNow . _TmTupleP
