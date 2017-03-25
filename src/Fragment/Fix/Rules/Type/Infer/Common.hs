{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Fragment.Fix.Rules.Type.Infer.Common (
    FixInferTypeConstraint
  , fixInferTypeInput
  ) where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Bound (Bound)
import Control.Lens (preview, review)
import Control.Monad.Except (MonadError)

import Ast.Type
import Ast.Type.Var
import Ast.Error.Common.Type
import Ast.Term
import Data.Bitransversable
import Data.Functor.Rec

import Fragment.TyArr.Ast.Type
import Fragment.TyArr.Ast.Error
import Fragment.Fix.Ast.Term

import Rules.Type.Infer.Common

import Rules.Type.Infer.SyntaxDirected (ITSyntax)
import Control.Monad.Except (MonadError)

import Rules.Type.Infer.Offline (ITOffline)
import Rules.Unification
import Control.Monad.State (MonadState)

class MkInferType i => FixInferTypeHelper i where
  type FixInferTypeHelperConstraint e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a i :: Constraint

  expectArr :: FixInferTypeHelperConstraint e w s r m ki ty a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy i
            -> Type ki ty a
            -> InferTypeMonad ki ty a m i (Type ki ty a, Type ki ty a)

instance FixInferTypeHelper ITSyntax where
  type FixInferTypeHelperConstraint e w s r m ki ty a ITSyntax =
    ( AsTyArr ki ty
    , MonadError e m
    , AsExpectedTyArr e ki ty a
    )

  expectArr _ _ =
    expectTyArr

instance FixInferTypeHelper ITOffline where
  type FixInferTypeHelperConstraint e w s r m ki ty a ITOffline =
    ( AsTyArr ki ty
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
    , Bound (ty ki)
    , Bitransversable (ty ki)
    )

  expectArr m i tyA = do
    tyP1 <- fmap (review _TyVar) freshTyVar
    tyP2 <- fmap (review _TyVar) freshTyVar
    expectTypeEq m i tyA (review _TyArr (tyP1, tyP2))
    return (tyP1, tyP2)

type FixInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , AsTmFix ki ty pt tm
  , AsTyArr ki ty
  , FixInferTypeHelper i
  , FixInferTypeHelperConstraint e w s r m ki ty a i
  , MonadError e (InferTypeMonad ki ty a m i)
  , AsExpectedTyArr e ki ty a
  )

fixInferTypeInput :: FixInferTypeConstraint e w s r m ki ty pt tm a i
                  => Proxy (MonadProxy e w s r m)
                  -> Proxy i
                  -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
fixInferTypeInput m i =
  InferTypeInput
    [] [ InferTypeRecurse $ inferTmFix m i] []

inferTmFix :: FixInferTypeConstraint e w s r m ki ty pt tm a i
           => Proxy (MonadProxy e w s r m )
           -> Proxy i
           -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmFix m i inferFn tm = do
  tmF <- preview _TmFix tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectArr m i tyF
    expectTypeEq m i tyArg tyRet
    return tyRet
