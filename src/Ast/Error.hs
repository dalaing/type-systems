{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Ast.Error (
    ErrSum(..)
  , _ErrNow
  , _ErrNext
  ) where

import Control.Lens.Prism (Prism', prism)

data ErrSum (f :: [*]) (ty :: k1) (pt :: k2) (tm :: k3) (a :: k4) where
  ErrNext :: ErrSum g ty pt tm a -> ErrSum (f ': g) ty pt tm a
  ErrNow :: f -> ErrSum (f ': g) ty pt tm a

_ErrNext :: Prism' (ErrSum (f ': g) ty pt tm a) (ErrSum g ty pt tm a)
_ErrNext = prism ErrNext $ \x -> case x of
  ErrNext y -> Right y
  _ -> Left x

_ErrNow :: Prism' (ErrSum (f ': g) ty pt tm a) f
_ErrNow = prism ErrNow $ \x -> case x of
  ErrNow y -> Right y
  _ -> Left x

instance Eq (ErrSum '[] ty pt tm a) where
  _ == _ = True

instance (Eq x, Eq ((ErrSum xs) ty pt tm a)) => Eq (ErrSum (x ': xs) ty pt tm a) where
  ErrNow a1 == ErrNow a2 = a1 == a2
  ErrNext n1 == ErrNext n2 = n1 == n2
  _ == _ = False

instance Ord (ErrSum '[] ty pt tm a) where
  compare _ _ = EQ

instance (Ord x, Ord ((ErrSum xs) ty pt tm a)) => Ord (ErrSum (x ': xs) ty pt tm a) where
  compare (ErrNow a1) (ErrNow a2) = compare a1 a2
  compare (ErrNow _) _ = LT
  compare _ (ErrNow _) = GT
  compare (ErrNext n1) (ErrNext n2) = compare n1 n2

instance Show ((ErrSum '[]) ty pt tm a) where
  showsPrec _ _ = id

instance (Show x, Show ((ErrSum xs) ty pt tm a)) => Show (ErrSum (x ': xs) ty pt tm a) where
  showsPrec m (ErrNow a) = showString "ErrSum " . showsPrec m a
  showsPrec m (ErrNext n) = showsPrec m n
