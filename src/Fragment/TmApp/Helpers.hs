{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.TmApp.Helpers (
    tmApp
  ) where

import Control.Lens (review)

import Ast.Term

import Fragment.TmApp.Ast.Term

tmApp :: AsTmApp ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmApp = curry $ review _TmApp
