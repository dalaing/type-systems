{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.SystemFw.Rules.Type.Infer.SyntaxDirected (
    SystemFwInferTypeConstraint
  , systemFwInferTypeInput
  ) where

import Data.Proxy (Proxy(..))

import Bound (abstract1, instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)
import Data.Functor.Classes (Eq1)

import Rules.Kind.Infer.SyntaxDirected
import Rules.Type.Infer.Common
import Rules.Type.Infer.SyntaxDirected
import Ast.Kind
import Ast.Type
import Ast.Type.Var
import Ast.Term
import Ast.Term.Var
import Ast.Error.Common
import Context.Type
import Context.Term
import Data.Functor.Rec

import Fragment.SystemFw.Ast.Type
import Fragment.SystemFw.Ast.Error
import Fragment.SystemFw.Ast.Term

inferTmLam :: SystemFwInferTypeConstraint e w s r m ki ty pt tm a
           => Proxy (MonadProxy e w s r m)
           -> (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmLam _ inferFn tm = do
  (tyArg, s) <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: SystemFwInferTypeConstraint e w s r m ki ty pt tm a
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
    expectTypeEq m (Proxy :: Proxy ISyntax) tyArg tyX
    return tyRet

inferTmLamTy :: SystemFwInferTypeConstraint e w s r m ki ty pt tm a
             => Proxy (MonadProxy e w s r m)
             -> (Term ki ty pt tm a -> m (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (m (Type ki ty a))
inferTmLamTy _ inferFn tm = do
  (ki, tmF) <- preview _TmLamTy tm
  return $ do
    v <- freshTyVar
    ty <- local (typeContext %~ insertType v ki) $ inferFn (review _Wrapped . instantiate1 (review (_AVar . _ATyVar) v) $ tmF)
    return . review _TyAll $ (ki, abstract1 v ty)

inferTmAppTy :: SystemFwInferTypeConstraint e w s r m ki ty pt tm a
             => Proxy (MonadProxy e w s r m)
             -> (Type ki ty a -> m (Kind ki))
             -> (Term ki ty pt tm a -> m (Type ki ty a))
             -> Term ki ty pt tm a
             -> Maybe (m (Type ki ty a))
inferTmAppTy _ inferKindFn inferTypeFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferTypeFn tmF
    (ki, s) <- expectTyAll tyF
    mkCheckKind inferKindFn tyX ki
    return $ instantiate1 tyX s

type SystemFwInferTypeConstraint e w s r m ki ty pt tm a = (Ord a, EqRec (ty ki), Eq1 ki, MonadState s m, HasTmVarSupply s, ToTmVar a, HasTyVarSupply s, ToTyVar a, MonadReader r m, HasTypeContext r ki a, HasTermContext r ki ty a, AsTySystemFw ki ty, MonadError e m, AsUnexpectedKind e ki, AsExpectedTypeEq e ki ty a, AsExpectedTyArr e ki ty a, AsExpectedTyAll e ki ty a, AsTmSystemFw ki ty pt tm, AsUnknownTypeError e, AsUnexpectedType e ki ty a, AsExpectedTypeAllEq e ki ty a)

systemFwInferTypeInput :: SystemFwInferTypeConstraint e w s r m ki ty pt tm a
                       => Proxy (MonadProxy e w s r m)
                       -> Proxy i
                       -> InferTypeInput e w s r m m ki ty pt tm a
systemFwInferTypeInput m _ =
  InferTypeInput
    []
    [ InferTypeRecurse $ inferTmLam m
    , InferTypeRecurse $ inferTmLamTy m
    , InferTypeRecurse $ inferTmApp m
    , InferTypeRecurseKind $ inferTmAppTy m
    ]
    []
