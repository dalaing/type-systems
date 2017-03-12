{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Fix.Helpers (
    tmFix
  ) where

import Control.Lens (review)

import Ast.Term

import Fragment.Fix.Ast.Term

tmFix :: AsTmFix ki ty pt tm
      => Term ki ty pt tm a
      -> Term ki ty pt tm a
tmFix = review _TmFix
