{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Annotation.Helpers (
    tmAnnotation
  ) where

import Control.Lens (review)

import Ast.Type
import Ast.Term

import Fragment.Annotation.Ast.Term

tmAnnotation :: AsTmAnnotation ki ty pt tm
             => Type ki ty a
             -> Term ki ty pt tm a
             -> Term ki ty pt tm a
tmAnnotation =
  curry $ review _TmAnnotation
