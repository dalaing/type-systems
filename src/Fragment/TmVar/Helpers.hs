{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.TmVar.Helpers (
    tmVar
  ) where

import Control.Lens (review)

import Ast.Term

tmVar :: a -> Term ty pt tm a
tmVar = review _TmVar
