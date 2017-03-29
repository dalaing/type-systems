{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.KiBase.Helpers (
    kiBase
  ) where

import Control.Lens (review)

import Ast.Kind

import Fragment.KiBase.Ast.Kind

kiBase :: AsKiBase k => Kind k a
kiBase = review _KiBase ()
