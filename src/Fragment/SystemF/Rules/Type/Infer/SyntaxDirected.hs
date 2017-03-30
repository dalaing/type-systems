{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fragment.SystemF.Rules.Type.Infer.SyntaxDirected (
    SystemFInferTypeConstraint
  , systemFInferTypeInput
  ) where

import Data.Proxy (Proxy(..))

import Bound (abstract1, instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)

import Rules.Type.Infer.Common
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

inferTmLam :: SystemFInferTypeConstraint e w s r m ki ty pt tm a
           => Proxy (MonadProxy e w s r m)
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmLam _ inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_TmAstVar . _TmAstTmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: SystemFInferTypeConstraint e w s r m ki ty pt tm a
           => Proxy (MonadProxy e w s r m)
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmApp m inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectTypeEq m (Proxy :: Proxy ITSyntax) tyArg tyX
    return tyRet

inferTmLamTy :: SystemFInferTypeConstraint e w s r m ki ty pt tm a
             => Proxy (MonadProxy e w s r m)
             -> (Term ki ty pt tm a -> m (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (m (Type ki ty a))
inferTmLamTy _ inferFn tm = do
  tmF <- preview _TmLamTy tm
  return $ do
    v <- freshTyVar
    ty <- inferFn (review _Wrapped . instantiate1 (review (_TmAstVar . _TmAstTyVar) v) $ tmF)
    return . review _TyAll . abstractTy v $ ty

inferTmAppTy :: SystemFInferTypeConstraint e w s r m ki ty pt tm a
             => Proxy (MonadProxy e w s r m)
             -> (Term ki ty pt tm a -> m (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (m (Type ki ty a))
inferTmAppTy _ inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    s <- expectTyAll tyF
    return $ instantiateTy tyX s

type SystemFInferTypeConstraint e w s r m ki ty pt tm a = (Ord a, EqRec ki, EqRec (ty ki), MonadState s m, HasTmVarSupply s, ToTmVar a, HasTyVarSupply s, ToTyVar a, MonadReader r m, HasTermContext r ki ty a, AsTySystemF ki ty, MonadError e m, AsExpectedTypeEq e ki ty a, AsExpectedTyArr e ki ty a, AsExpectedTyAll e ki ty a, AsTmSystemF ki ty pt tm, AsUnknownTypeError e, AsUnexpectedType e ki ty a, AsExpectedTypeAllEq e ki ty a)

systemFInferTypeInput :: SystemFInferTypeConstraint e w s r m ki ty pt tm a
                      => Proxy (MonadProxy e w s r m)
                      -> Proxy i
                      -> InferTypeInput e w s r m m ki ty pt tm a
systemFInferTypeInput m _ =
  InferTypeInput
    []
    [ InferTypeRecurse $ inferTmLam m
    , InferTypeRecurse $ inferTmLamTy m
    , InferTypeRecurse $ inferTmApp m
    , InferTypeRecurse $ inferTmAppTy m
    ]
    []
