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
module Fragment.Bool.Ast.Term (
    TmFBool
  , AsTmBool(..)
  ) where

import Data.Functor.Classes (showsUnaryWith, showsBinaryWith)

import Bound (Bound(..))
import Control.Lens.Iso (bimapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Term
import Util

data TmFBool (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmBoolF Bool
  | TmAndF (f a) (f a)
  | TmOrF (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFBool

deriveEq1 ''TmFBool
deriveOrd1 ''TmFBool
deriveShow1 ''TmFBool

instance EqRec (TmFBool ty pt) where
  liftEqRec _ _ (TmBoolF i) (TmBoolF j) = i == j
  liftEqRec eR _ (TmAndF x1 y1) (TmAndF x2 y2) = eR x1 x2 && eR y1 y2
  liftEqRec eR _ (TmOrF x1 y1) (TmOrF x2 y2) = eR x1 x2 && eR y1 y2
  liftEqRec _ _ _ _ = False

instance OrdRec (TmFBool ty pt) where
  liftCompareRec _ _ (TmBoolF i) (TmBoolF j) = compare i j
  liftCompareRec _ _ (TmBoolF _) _ = LT
  liftCompareRec _ _ _ (TmBoolF _) = GT
  liftCompareRec cR _ (TmAndF x1 y1) (TmAndF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z
  liftCompareRec _ _ (TmAndF _ _) _ = LT
  liftCompareRec _ _ _ (TmAndF _ _) = GT
  liftCompareRec cR _ (TmOrF x1 y1) (TmOrF x2 y2) =
    case cR x1 x2 of
      EQ -> cR y1 y2
      z -> z

instance ShowRec (TmFBool ty pt) where
  liftShowsPrecRec _ _ _ _ n (TmBoolF i) =
    showsUnaryWith showsPrec "TmBoolF" n i
  liftShowsPrecRec sR _ _ _ n (TmAndF x y) =
    showsBinaryWith sR sR "TmAndF" n x y
  liftShowsPrecRec sR _ _ _ n (TmOrF x y) =
    showsBinaryWith sR sR "TmOrF" n x y

instance Bound (TmFBool ty pt) where
  TmBoolF b >>>= _ = TmBoolF b
  TmAndF x y >>>= f = TmAndF (x >>= f) (y >>= f)
  TmOrF x y >>>= f = TmOrF (x >>= f) (y >>= f)

instance Bitransversable (TmFBool ty pt) where
  bitransverse _ _ (TmBoolF b) = pure $ TmBoolF b
  bitransverse fT fL (TmAndF x y) = TmAndF <$> fT fL x <*> fT fL y
  bitransverse fT fL (TmOrF x y) = TmOrF <$> fT fL x <*> fT fL y

class AsTmBool ty pt tm where
  _TmBoolP :: Prism' (tm ty pt k a) (TmFBool ty pt k a)

  _TmBool :: Prism' (Term ty pt tm a) Bool
  _TmBool = _Wrapped . _ATerm . _TmBoolP . _TmBoolF

  _TmAnd :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmAnd = _Wrapped . _ATerm . _TmBoolP . _TmAndF . bimapping _Unwrapped _Unwrapped

  _TmOr :: Prism' (Term ty pt tm a) (Term ty pt tm a, Term ty pt tm a)
  _TmOr = _Wrapped . _ATerm . _TmBoolP . _TmOrF . bimapping _Unwrapped _Unwrapped

instance AsTmBool ty pt TmFBool where
  _TmBoolP = id

instance {-# OVERLAPPABLE #-} AsTmBool ty pt (TmSum xs) => AsTmBool ty pt (TmSum (x ': xs)) where
  _TmBoolP = _TmNext . _TmBoolP

instance {-# OVERLAPPING #-} AsTmBool ty pt (TmSum (TmFBool ': xs)) where
  _TmBoolP = _TmNow . _TmBoolP
