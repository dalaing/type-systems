{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Fragment.Annotation.Rules.Type.Infer.Common (
    AnnotationHelper(..)
  , inferTypeInput
  ) where

import Control.Lens (preview)

import Ast.Type
import Ast.Error.Common
import Ast.Term
import Rules.Type.Infer.Common

import Fragment.Annotation.Ast.Term

data AnnotationHelper m ki ty a =
  AnnotationHelper {
    ahExpectType :: ExpectedType ki ty a -> ActualType ki ty a -> m ()
  }

inferTypeInput :: (Monad mi, AsTmAnnotation ki ty pt tm)
               => AnnotationHelper mi ki ty a
               -> InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput ah =
  InferTypeInput
    [] [ InferTypeRecurse $ inferTmAnnotation ah ] []

inferTmAnnotation :: (Monad m, AsTmAnnotation ki ty pt tm)
            => AnnotationHelper m ki ty a
            -> (Term ki ty pt tm a -> m (Type ki ty a))
            -> Term ki ty pt tm a
            -> Maybe (m (Type ki ty a))
inferTmAnnotation (AnnotationHelper expectType) inferFn tm = do
  (tyE, tmAnn) <- preview _TmAnnotation tm
  return $ do
    tyA <- inferFn tmAnn
    expectType (ExpectedType tyE) (ActualType tyA)
    return tyE
