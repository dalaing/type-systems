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
module Fragment.Int.Ast.Type (
    TyFInt
  , AsTyInt(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFInt (ki :: (* -> *) -> * -> *) (f :: * -> *) a =
  TyIntF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFInt

deriveEq1 ''TyFInt
deriveOrd1 ''TyFInt
deriveShow1 ''TyFInt

instance EqRec (TyFInt ki) where
  liftEqRec _ _ TyIntF TyIntF = True

instance OrdRec (TyFInt ki) where
  liftCompareRec _ _ TyIntF TyIntF = EQ

instance ShowRec (TyFInt ki) where
  liftShowsPrecRec _ _ _ _ n TyIntF = showsPrec n TyIntF

instance Bound (TyFInt ki) where
  TyIntF >>>= _ = TyIntF

instance Bitransversable (TyFInt ki) where
  bitransverse _ _ TyIntF = pure TyIntF

class AsTyInt ki ty where
  _TyIntP :: Prism' (ty ki j a) (TyFInt ki j a)

  _TyInt :: Prism' (Type ki ty a) ()
  _TyInt = _Wrapped . _TyAstType . _TyIntP . _TyIntF

instance AsTyInt ki TyFInt where
  _TyIntP = id

instance {-# OVERLAPPABLE #-} AsTyInt ki (TySum xs) => AsTyInt ki (TySum (x ': xs)) where
  _TyIntP = _TyNext . _TyIntP

instance {-# OVERLAPPING #-} AsTyInt ki (TySum (TyFInt ': xs)) where
  _TyIntP = _TyNow . _TyIntP
