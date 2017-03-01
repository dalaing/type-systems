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
module Fragment.If.Ast.Term (
    TmFIf
  , AsTmIf(..)
  ) where

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec
import Util.Prisms

data TmFIf (k :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
    TmIfF (f a) (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFIf

deriveEq1 ''TmFIf
deriveOrd1 ''TmFIf
deriveShow1 ''TmFIf

instance EqRec (TmFIf ki ty pt) where
  liftEqRec eR _ (TmIfF x1 y1 z1) (TmIfF x2 y2 z2) =
    eR x1 x2 && eR y1 y2 && eR z1 z2

instance OrdRec (TmFIf ki ty pt) where
  liftCompareRec cR _ (TmIfF x1 y1 z1) (TmIfF x2 y2 z2) =
    case cR x1 x2 of
      EQ -> case cR y1 y2 of
        EQ -> cR z1 z2
        z -> z
      z -> z

instance ShowRec (TmFIf ki ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmIfF x y z) =
    showString "if " .
    sR n x .
    showString " then " .
    sR n y .
    showString " else " .
    sR n z

instance Bound (TmFIf ki ty pt) where
  TmIfF b t e >>>= f = TmIfF (b >>= f) (t >>= f) (e >>= f)

instance Bitransversable (TmFIf ki ty pt) where
  bitransverse fT fL (TmIfF x y z) = TmIfF <$> fT fL x <*> fT fL y <*> fT fL z

class AsTmIf ki ty pt tm where
  _TmIfP :: Prism' (tm ki ty pt f a) (TmFIf ki ty pt f a)

  _TmIf :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a, Term ki ty pt tm a, Term ki ty pt tm a)
  _TmIf = _Wrapped . _ATerm . _TmIfP . _TmIfF . mkTriple _Unwrapped _Unwrapped _Unwrapped

instance AsTmIf ki ty pt TmFIf where
  _TmIfP = id

instance {-# OVERLAPPABLE #-} AsTmIf ki ty pt (TmSum xs) => AsTmIf ki ty pt (TmSum (x ': xs)) where
  _TmIfP = _TmNext . _TmIfP

instance {-# OVERLAPPING #-} AsTmIf ki ty pt (TmSum (TmFIf ': xs)) where
  _TmIfP = _TmNow . _TmIfP
