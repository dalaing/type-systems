{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.PtVar.Helpers (
    ptVar
  ) where

import Control.Lens (review)

import Ast.Pattern

ptVar :: a -> Pattern pt a
ptVar = review _PtVar
