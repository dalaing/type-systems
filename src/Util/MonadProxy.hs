{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE KindSignatures #-}
module Util.MonadProxy (
    MonadProxy
  ) where

data MonadProxy (e :: *) (w :: *) (s :: *) (r :: *) (m :: * -> *)
