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
module Fragment.Variant.Ast.Term (
    TmFVariant
  , AsTmVariant(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import qualified Data.Text as T

import Ast.Term
import Ast.Type
import Util

data TmFVariant (ty :: (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmVariantF T.Text (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFVariant

deriveEq1 ''TmFVariant
deriveOrd1 ''TmFVariant
deriveShow1 ''TmFVariant

instance EqRec (TmFVariant ty pt) where
  liftEqRec eR _ (TmVariantF t1 tm1 ty1) (TmVariantF t2 tm2 ty2) =
    t1 == t2 && eR tm1 tm2 && eR ty1 ty2

instance OrdRec (TmFVariant ty pt) where
  liftCompareRec cR _ (TmVariantF t1 tm1 ty1) (TmVariantF t2 tm2 ty2) =
    case compare t1 t2 of
      EQ -> case cR tm1 tm2 of
        EQ -> cR ty1 ty2
        z -> z
      z -> z

instance ShowRec (TmFVariant ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmVariantF t tm ty) =
    showString "TmFVariant " .
    showString (T.unpack t) .
    showString " " .
    sR n tm .
    showString " " .
    sR n ty

instance Bound (TmFVariant ty pt) where
  TmVariantF t tm ty >>>= f = TmVariantF t (tm >>= f) (ty >>= f)

instance Bitransversable (TmFVariant ty pt) where
  bitransverse fT fL (TmVariantF l tm ty) = TmVariantF <$> pure l <*> fT fL tm <*> fT fL ty

class AstTransversable ty pt tm => AsTmVariant ty pt tm where
  _TmVariantP :: Prism' (tm ty pt k a) (TmFVariant ty pt k a)

  _TmVariant :: Prism' (Term ty pt tm a) (T.Text, Term ty pt tm a, Type ty a)
  _TmVariant = _Wrapped . _ATerm . _TmVariantP . _TmVariantF . mkTriple id _Unwrapped _Type

instance (Bitransversable ty, Bitransversable pt) => AsTmVariant ty pt TmFVariant where
  _TmVariantP = id

instance {-# OVERLAPPABLE #-} (Bitransversable (x ty pt), AsTmVariant ty pt (TmSum xs)) => AsTmVariant ty pt (TmSum (x ': xs)) where
  _TmVariantP = _TmNext . _TmVariantP

instance {-# OVERLAPPING #-} (Bitransversable ty, Bitransversable pt, Bitransversable (TmSum xs ty pt)) => AsTmVariant ty pt (TmSum (TmFVariant ': xs)) where
  _TmVariantP = _TmNow . _TmVariantP
