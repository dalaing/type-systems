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
module Fragment.Bool.Ast.Pattern (
    PtFBool
  , AsPtBool(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Pattern
import Util

data PtFBool (f :: * -> *) a =
  PtBoolF Bool
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''PtFBool

deriveEq1 ''PtFBool
deriveOrd1 ''PtFBool
deriveShow1 ''PtFBool

instance EqRec PtFBool where
  liftEqRec _ _ (PtBoolF i) (PtBoolF j) = i == j

instance OrdRec PtFBool where
  liftCompareRec _ _ (PtBoolF i) (PtBoolF j) = compare i j

instance ShowRec PtFBool where
  liftShowsPrecRec _ _ _ _ = showsPrec

instance Bound PtFBool where
  PtBoolF b >>>= _ = PtBoolF b

instance Bitransversable PtFBool where
  bitransverse _ _ (PtBoolF b) = pure $ PtBoolF b

class AsPtBool pt where
  _PtBoolP :: Prism' (pt k a) (PtFBool k a)

  _PtBool :: Prism' (Pattern pt a) Bool
  _PtBool = _PtTree . _PtBoolP . _PtBoolF

instance AsPtBool PtFBool where
  _PtBoolP = id

instance {-# OVERLAPPABLE #-} AsPtBool (PtSum xs) => AsPtBool (PtSum (x ': xs)) where
  _PtBoolP = _PtNext . _PtBoolP

instance {-# OVERLAPPING #-} AsPtBool (PtSum (PtFBool ': xs)) where
  _PtBoolP = _PtNow . _PtBoolP
