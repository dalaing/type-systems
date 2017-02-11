{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Typed.SystemF.Type.Class (
    AsType (..)
  ) where

import Control.Lens

class AsType ty where
  _TyVar :: Prism' (ty a) a
  _TyAll :: Eq a => Prism' (a, ty a) (a, ty a)
  _TyArr :: Prism' (ty a) (ty a, ty a)
  _TyInt :: Prism' (ty a) ()

  substTyAll :: ty a -> ty a -> Maybe (ty a)
