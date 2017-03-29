{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.TmLam.Rules.Type.Infer.Common (
    TmLamInferTypeConstraint
  , TmLamInferTypeHelper(..)
  , tmLamInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Scope, instantiate1)
import Control.Lens (preview, review, (%~))
import Control.Lens.Wrapped (_Wrapped)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Except (MonadError)
import Control.Monad.Error.Lens (throwing)

import Ast.Type
import Ast.Error.Common.Type
import Ast.Type.Var
import Ast.Term
import Ast.Term.Var
import Context.Term
import Data.Functor.Rec

import Fragment.TyArr.Ast.Type
import Fragment.TmLam.Ast.Error
import Fragment.TmLam.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)
import Rules.Type.Infer.Offline (ITOffline)
import Rules.Unification

class MkInferType i => TmLamInferTypeHelper i where
  type TmLamInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: (* -> *) -> * -> *) (ty :: ((* -> *) -> * -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: (((* -> *) -> * -> *) -> (((* -> *) -> * -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a i :: Constraint

  expectTmLam :: TmLamInferTypeHelperConstraint e w s r m ki ty pt tm a i
              => Proxy (MonadProxy e w s r m)
              -> Proxy i
              -> Term ki ty pt tm a
              -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a, Scope () (TmAst ki ty pt tm) (TmAstVar a)))

instance TmLamInferTypeHelper ITSyntax where
  type TmLamInferTypeHelperConstraint e w s r m ki ty pt tm a ITSyntax =
    ( AsTmLam ki ty pt tm
    , MonadError e m
    , AsExpectedTmLamAnnotation e
    )

  expectTmLam _ _ tm = do
    (mty, s) <- preview _TmLam tm
    return $ do
      case mty of
        Nothing -> throwing _ExpectedTmLamAnnotation ()
        Just ty -> return (ty, s)

instance TmLamInferTypeHelper ITOffline where
  type TmLamInferTypeHelperConstraint e w s r m ki ty pt tm a ITOffline =
    ( AsTmLam ki ty pt tm
    , MonadState s m
    , HasTyVarSupply s
    , ToTyVar a
    , Ord a
    , OrdRec (ty ki)
    , MonadError e m
    , AsUnknownTypeError e
    , AsOccursError e (Type ki ty) a
    , AsUnificationMismatch e (Type ki ty) a
    , AsUnificationExpectedEq e (Type ki ty) a
    )

  expectTmLam m i tm = do
    (mty, s) <- preview _TmLam tm
    return $ do
      tyV <- fmap (review _TyVar) freshTyVar
      case mty of
        Nothing -> return ()
        Just ty -> expectType m i (ExpectedType ty) (ActualType tyV)
      return (tyV, s)

type TmLamInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , TmLamInferTypeHelper i
  , TmLamInferTypeHelperConstraint e w s r m ki ty pt tm a i
  , AsTmLam ki ty pt tm
  , AsTyArr ki ty
  , Ord a
  , MonadReader r (InferTypeMonad ki ty a m i)
  , HasTermContext r ki ty a
  , MonadState s (InferTypeMonad ki ty a m i)
  , HasTmVarSupply s
  , ToTmVar a
  )

inferTmLam :: TmLamInferTypeConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m)
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmLam m i inferFn tm = do
  act <- expectTmLam m i tm
  return $ do
    (tyArg, s) <- act
    v <- freshTmVar
    let tmF = review _Wrapped $ instantiate1 (review (_TmAstVar . _TmAstTmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

tmLamInferTypeInput :: TmLamInferTypeConstraint e w s r m ki ty pt tm a i
                    => Proxy (MonadProxy e w s r m)
                    -> Proxy i
                    -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
tmLamInferTypeInput m i =
  InferTypeInput
    []
    [InferTypeRecurse $ inferTmLam m i]
    []
