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
module Fragment.Fix.Ast.Term (
    TmFFix
  , AsTmFix(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)

import Bound (Bound(..))
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

data TmFFix (k :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) f a =
  TmFixF (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''TmFFix

deriveEq1 ''TmFFix
deriveOrd1 ''TmFFix
deriveShow1 ''TmFFix

instance EqRec (TmFFix ki ty pt) where
  liftEqRec eR _ (TmFixF x1) (TmFixF x2) =
    eR x1 x2

instance OrdRec (TmFFix ki ty pt) where
  liftCompareRec cR _ (TmFixF x1) (TmFixF x2) =
    cR x1 x2

instance ShowRec (TmFFix ki ty pt) where
  liftShowsPrecRec sR _ _ _ n (TmFixF x) =
    showsUnaryWith sR "TmFixF" n x

instance Bound (TmFFix ki ty pt) where
  TmFixF x >>>= f = TmFixF (x >>= f)

instance Bitransversable (TmFFix ki ty pt) where
  bitransverse fT fL (TmFixF x) = TmFixF <$> fT fL x

class AsTmFix ki ty pt tm where
  _TmFixP :: Prism' (tm ki ty pt f a) (TmFFix ki ty pt f a)

  _TmFix :: Prism' (Term ki ty pt tm a) (Term ki ty pt tm a)
  _TmFix = _Wrapped . _TmAstTerm . _TmFixP . _TmFixF . _Unwrapped

instance AsTmFix ki ty pt TmFFix where
  _TmFixP = id

instance {-# OVERLAPPABLE #-} AsTmFix ki ty pt (TmSum xs) => AsTmFix ki ty pt (TmSum (x ': xs)) where
  _TmFixP = _TmNext . _TmFixP

instance {-# OVERLAPPING #-} AsTmFix ki ty pt (TmSum (TmFFix ': xs)) where
  _TmFixP = _TmNow . _TmFixP
