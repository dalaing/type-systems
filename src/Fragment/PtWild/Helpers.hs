{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.PtWild.Helpers (
    ptWild
  ) where

import Control.Lens (review)

import Ast.Pattern

import Fragment.PtWild.Ast.Pattern

ptWild :: AsPtWild pt => Pattern pt a
ptWild = review _PtWild ()
