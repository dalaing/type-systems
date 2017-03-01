{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.If.Helpers (
    tmIf
  ) where

import Control.Lens (review)

import Ast.Term

import Fragment.If.Ast.Term

tmIf :: AsTmIf ki ty pt tm => Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a -> Term ki ty pt tm a
tmIf b t f = review _TmIf (b, t, f)
