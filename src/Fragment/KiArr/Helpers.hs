{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.KiArr.Helpers (
    kiArr
  ) where

import Control.Lens (review)

import Ast.Kind

import Fragment.KiArr.Ast.Kind

kiArr :: AsKiArr ki => Kind ki -> Kind ki -> Kind ki
kiArr = curry $ review _KiArr
