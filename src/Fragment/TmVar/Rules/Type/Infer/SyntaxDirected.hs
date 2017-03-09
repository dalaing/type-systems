{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmVar.Rules.Type.Infer.SyntaxDirected (
    TmVarInferTypeContext
  , tmVarInferTypeRules
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type
import Ast.Term
import Context.Term

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, HasTermContext r ki ty a, AsUnboundTermVariable e a) => Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

type TmVarInferTypeContext e w s r m ki ty pt tm a = (InferTypeContext e w s r m ki ty pt tm a, Ord a, MonadReader r m, HasTermContext r ki ty a, MonadError e m, AsUnboundTermVariable e a)

tmVarInferTypeRules :: TmVarInferTypeContext e w s r m ki ty pt tm a
                => InferTypeInput e w s r m m ki ty pt tm a
tmVarInferTypeRules =
  InferTypeInput [] [ InferTypeBase inferTmVar ] []

