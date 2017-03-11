{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.TyArr.Helpers (
    tyArr
  ) where

import Control.Lens (review)

import Ast.Type

import Fragment.TyArr.Ast.Type

tyArr :: AsTyArr ki ty => Type ki ty a -> Type ki ty a -> Type ki ty a
tyArr = curry $ review _TyArr
