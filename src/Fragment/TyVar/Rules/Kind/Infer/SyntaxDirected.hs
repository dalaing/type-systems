{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.TyVar.Rules.Kind.Infer.SyntaxDirected (
    TyVarKindRulesContext
  , tyVarKindRules
  ) where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Lens (preview)

import Rules.Kind.Infer.SyntaxDirected
import Ast.Kind
import Ast.Type
import Context.Type

inferTyVar :: (Ord a, MonadReader r m, MonadError e m, HasTypeContext r ki a, AsUnboundTypeVariable e a) => Type ki ty a -> Maybe (m (Kind ki))
inferTyVar ty = do
  v <- preview _TyVar ty
  return $ lookupType v

type TyVarKindRulesContext e w s r m ki ty a = (KindRulesContext e w s r m ki ty a, Ord a, MonadReader r m, HasTypeContext r ki a, MonadError e m, AsUnboundTypeVariable e a)

tyVarKindRules :: TyVarKindRulesContext e w s r m ki ty a
                => KindRulesInput e w s r m ki ty a
tyVarKindRules =
  KindRulesInput
    [ InferKindBase inferTyVar ]
