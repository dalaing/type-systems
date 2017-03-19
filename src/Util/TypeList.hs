{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.TypeList (
    TLAppend(..)
  ) where

class TLAppend (xs :: [k]) (ys :: [k]) where
  type Append xs ys :: [k]

instance TLAppend '[] ys where
  type Append '[] ys = ys

instance TLAppend xs ys => TLAppend (x ': xs) ys where
  type Append (x ': xs) ys = x ': (Append xs ys)
