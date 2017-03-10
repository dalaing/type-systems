{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmVar.Rules.Type.Infer.Common (
    inferTypeInput
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Ast.Type
import Ast.Term
import Context.Term

import Rules.Type.Infer.Common

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ki ty a, AsUnboundTermVariable e a) => Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

inferTypeInput :: (Ord a, MonadReader r mi, MonadError e mi, HasTermContext r ki ty a, AsUnboundTermVariable e a)
               => InferTypeInput e w s r m mi ki ty pt tm a
inferTypeInput =
  InferTypeInput [] [ InferTypeBase inferTmVar ] []
