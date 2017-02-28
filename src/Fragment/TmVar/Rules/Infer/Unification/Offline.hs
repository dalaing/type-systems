{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TmVar.Rules.Infer.Unification.Offline (
    TmVarInferContext
  , tmVarInferRules
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Rules.Infer.Unification.Offline
import Ast.Type
import Ast.Term
import Context.Term

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ty a, AsUnboundTermVariable e a) => Term ty pt tm a -> Maybe (m (Type ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

type TmVarInferContext e w s r m ty pt tm a = (InferContext e w s r m ty pt tm a, Ord a, MonadReader r m, HasTermContext r ty a, MonadError e m, AsUnboundTermVariable e a)

tmVarInferRules :: TmVarInferContext e w s r m ty pt tm a
                => InferInput e w s r m ty pt tm a
tmVarInferRules =
  InferInput
    []
    []
    [ InferBase inferTmVar ]
    []
