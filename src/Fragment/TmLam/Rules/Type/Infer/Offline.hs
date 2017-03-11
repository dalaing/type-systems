{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmLam.Rules.Type.Infer.Offline (
    TmLamInferTypeContext
  , tmLamInferTypeRules
  ) where

import Bound (Scope)
import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadState)

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common
import Ast.Term
import Data.Functor.Rec
import Rules.Type.Infer.Offline

import Fragment.TyArr.Ast.Error
import Fragment.TmLam.Ast.Term

import qualified Fragment.TmLam.Rules.Type.Infer.Common as L

type TmLamInferTypeContext e w s r m ki ty pt tm a =
  ( L.TmLamInferTypeContext e w s r m (UnifyT ki ty a m) ki ty pt tm a
  , Eq a
  , EqRec (ty ki)
  , MonadState s m
  , HasTyVarSupply s
  , ToTyVar a
  )

expectTmLam :: TmLamInferTypeContext e w s r m ki ty pt tm a
            => Term ki ty pt tm a
            -> Maybe (UnifyT ki ty a m (Type ki ty a, Scope () (Ast ki ty pt tm) (AstVar a)))
expectTmLam tm = do
  (mty, s) <- preview _TmLam tm
  return $ do
    tyV <- fmap (review _TyVar) freshTyVar
    case mty of
      Nothing -> return ()
      Just ty -> expectType (ExpectedType ty) (ActualType tyV)
    return (tyV, s)

tmLamInferTypeRules :: TmLamInferTypeContext e w s r m ki ty pt tm a
                    => InferTypeInput e w s r m (UnifyT ki ty a m) ki ty pt tm a
tmLamInferTypeRules =
  L.inferTypeInput (L.TmLamHelper expectTmLam)
