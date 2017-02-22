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
module Fragment.Int.Ast.Type (
    TyFInt
  , AsTyInt(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Util

data TyFInt (f :: * -> *) a =
  TyIntF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFInt

deriveEq1 ''TyFInt
deriveOrd1 ''TyFInt
deriveShow1 ''TyFInt

instance EqRec TyFInt where
  liftEqRec _ _ TyIntF TyIntF = True

instance OrdRec TyFInt where
  liftCompareRec _ _ TyIntF TyIntF = EQ

instance ShowRec TyFInt where
  liftShowsPrecRec _ _ _ _ n TyIntF = showsPrec n TyIntF

instance Bound TyFInt where
  TyIntF >>>= _ = TyIntF

instance Bitransversable TyFInt where
  bitransverse _ _ TyIntF = pure TyIntF

class AsTyInt ty where
  _TyIntP :: Prism' (ty k a) (TyFInt k a)

  _TyInt :: Prism' (Type ty a) ()
  _TyInt = _TyTree . _TyIntP . _TyIntF

instance AsTyInt TyFInt where
  _TyIntP = id

instance {-# OVERLAPPABLE #-} AsTyInt (TySum xs) => AsTyInt (TySum (x ': xs)) where
  _TyIntP = _TyNext . _TyIntP

instance {-# OVERLAPPING #-} AsTyInt (TySum (TyFInt ': xs)) where
  _TyIntP = _TyNow . _TyIntP
