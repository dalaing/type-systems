{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.SystemF.Rules.Infer (
    SystemFInferContext
  , systemFInferRules
  ) where

import Bound (Bound, abstract1, instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)

import Rules.Infer
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

equivArr :: AsTySystemF ty => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivArr equivFn ty1 ty2 = do
  (p1a, p1b) <- preview _TyArr ty1
  (p2a, p2b) <- preview _TyArr ty2
  return $ equivFn p1a p2a && equivFn p1b p2b

equivAll :: (Eq a, EqRec ty, AsTySystemF ty) => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivAll equivFn ty1 ty2 = do
  s1 <- preview _TyAll ty1
  s2 <- preview _TyAll ty2
  return $ liftEqRec equivFn (==) s1 s2

inferTmLam :: (Ord a, AstBound ty pt tm, MonadState s m, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTySystemF ty, AsTmSystemF ty pt tm, HasTermContext r ty a) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmLam inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq a, EqRec ty, MonadError e m, AsTySystemF ty, AsTmSystemF ty pt tm, AsExpectedTyArr e ty a, AsExpectedEq e ty a) => (Type ty a -> Type ty a -> Bool) -> (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmApp tyEquiv inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyEquiv tyArg tyX
    return tyRet

inferTmLamTy :: (Eq a, Bound ty, Bound pt, Bound (tm ty pt), MonadState s m, HasTyVarSupply s, ToTyVar a, AsTySystemF ty, AsTmSystemF ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmLamTy inferFn tm = do
  tmF <- preview _TmLamTy tm
  return $ do
    v <- freshTyVar
    ty <- inferFn (review _Wrapped . instantiate1 (review (_AVar . _ATyVar) v) $ tmF)
    return . review _TyAll . abstract1 v $ ty

inferTmAppTy :: (Bound ty, MonadError e m, AsExpectedTyAll e ty a , AsTySystemF ty, AsTmSystemF ty pt tm) => (Term ty pt tm a -> m (Type ty a)) -> Term ty pt tm a -> Maybe (m (Type ty a))
inferTmAppTy inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    s <- expectTyAll tyF
    return $ instantiate1 tyX s

type SystemFInferContext e w s r m ty pt tm a = (Ord a, InferContext e w s r m ty pt tm a, MonadState s m, HasTmVarSupply s, ToTmVar a, HasTyVarSupply s, ToTyVar a, MonadReader r m, HasTermContext r ty a, AsTySystemF ty, AsExpectedEq e ty a, AsExpectedTyArr e ty a, AsExpectedTyAll e ty a, AsTmSystemF ty pt tm)

systemFInferRules :: SystemFInferContext e w s r m ty pt tm a
                  => InferInput e w s r m ty pt tm a
systemFInferRules =
  InferInput
    [ EquivRecurse equivArr
    , EquivRecurse equivAll
    ]
    [ InferRecurse inferTmLam
    , InferRecurse inferTmLamTy
    , InferTyEquivRecurse inferTmApp
    , InferRecurse inferTmAppTy
    ]
    []
