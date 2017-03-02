{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.SystemF.Rules.Type.Infer.SyntaxDirected (
    SystemFInferContext
  , systemFInferRules
  ) where

import Bound (Bound, abstract1, instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)

import Rules.Type.Infer.SyntaxDirected
import Ast.Type
import Ast.Type.Var
import Ast.Term
import Ast.Term.Var
import Ast.Error.Common
import Context.Term
import Data.Functor.Rec

import Fragment.SystemF.Ast.Type
import Fragment.SystemF.Ast.Error
import Fragment.SystemF.Ast.Term

inferTmLam :: (Ord a, AstBound ki ty pt tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySystemF ki ty, AsTmSystemF ki ty pt tm, HasTermContext r ki ty a) => (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq a, EqRec (ty ki), MonadError e m, AsTySystemF ki ty, AsTmSystemF ki ty pt tm, AsExpectedTyArr e ki ty a, AsExpectedTypeEq e ki ty a)
           => (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectTypeEq tyArg tyX
    return tyRet

inferTmLamTy :: (Eq a, Bound (ty ki), Bound pt, Bound (tm ki ty pt), MonadState s m, HasTyVarSupply s, ToTyVar a, AsTySystemF ki ty, AsTmSystemF ki ty pt tm) => (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmLamTy inferFn tm = do
  tmF <- preview _TmLamTy tm
  return $ do
    v <- freshTyVar
    ty <- inferFn (review _Wrapped . instantiate1 (review (_AVar . _ATyVar) v) $ tmF)
    return . review _TyAll . abstract1 v $ ty

inferTmAppTy :: (Bound (ty ki), MonadError e m, AsExpectedTyAll e ki ty a , AsTySystemF ki ty, AsTmSystemF ki ty pt tm) => (Term ki ty pt tm a -> m (Type ki ty a)) -> Term ki ty pt tm a -> Maybe (m (Type ki ty a))
inferTmAppTy inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    s <- expectTyAll tyF
    return $ instantiate1 tyX s

type SystemFInferContext e w s r m ki ty pt tm a = (Ord a, InferContext e w s r m ki ty pt tm a, MonadState s m, HasTmVarSupply s, ToTmVar a, HasTyVarSupply s, ToTyVar a, MonadReader r m, HasTermContext r ki ty a, AsTySystemF ki ty, AsExpectedTypeEq e ki ty a, AsExpectedTyArr e ki ty a, AsExpectedTyAll e ki ty a, AsTmSystemF ki ty pt tm)

systemFInferRules :: SystemFInferContext e w s r m ki ty pt tm a
                  => InferInput e w s r m ki ty pt tm a
systemFInferRules =
  InferInput
    [ InferRecurse inferTmLam
    , InferRecurse inferTmLamTy
    , InferRecurse inferTmApp
    , InferRecurse inferTmAppTy
    ]
    []
