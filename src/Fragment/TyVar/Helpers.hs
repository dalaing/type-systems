{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.TyVar.Helpers (
    tyVar
  ) where

import Control.Lens (review)

import Ast.Type

tyVar :: a -> Type ki ty a
tyVar = review _TyVar
