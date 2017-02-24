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
module Fragment.Bool.Ast.Type (
    TyFBool
  , AsTyBool(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFBool (f :: * -> *) a =
  TyBoolF
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TyFBool

deriveEq1 ''TyFBool
deriveOrd1 ''TyFBool
deriveShow1 ''TyFBool

instance EqRec TyFBool where
  liftEqRec _ _ TyBoolF TyBoolF = True

instance OrdRec TyFBool where
  liftCompareRec _ _ TyBoolF TyBoolF = EQ

instance ShowRec TyFBool where
  liftShowsPrecRec _ _ _ _ n TyBoolF = showsPrec n TyBoolF

instance Bound TyFBool where
  TyBoolF >>>= _ = TyBoolF

instance Bitransversable TyFBool where
  bitransverse _ _ TyBoolF = pure TyBoolF

class AsTyBool ty where
  _TyBoolP :: Prism' (ty k a) (TyFBool k a)

  _TyBool :: Prism' (Type ty a) ()
  _TyBool = _TyTree . _TyBoolP . _TyBoolF

instance AsTyBool TyFBool where
  _TyBoolP = id

instance {-# OVERLAPPABLE #-} AsTyBool (TySum xs) => AsTyBool (TySum (x ': xs)) where
  _TyBoolP = _TyNext . _TyBoolP

instance {-# OVERLAPPING #-} AsTyBool (TySum (TyFBool ': xs)) where
  _TyBoolP = _TyNow . _TyBoolP
