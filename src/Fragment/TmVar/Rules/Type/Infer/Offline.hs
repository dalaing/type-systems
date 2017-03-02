{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TmVar.Rules.Type.Infer.Offline (
    TmVarInferContext
  , tmVarInferRules
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Rules.Type.Infer.Offline
import Ast.Type
import Ast.Term
import Context.Term

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ki ty a, AsUnboundTermVariable e a) => Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

type TmVarInferContext e w s r m ki ty pt tm a = (InferContext e w s r m ki ty pt tm a, Ord a, MonadReader r m, HasTermContext r ki ty a, MonadError e m, AsUnboundTermVariable e a)

tmVarInferRules :: TmVarInferContext e w s r m ki ty pt tm a
                => InferInput e w s r m ki ty pt tm a
tmVarInferRules =
  InferInput
    []
    [ InferBase inferTmVar ]
    []
