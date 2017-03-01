{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.HM.Rules.Infer.Unification.Offline (
    HMInferContext
  , hmInferRules
  ) where

import Bound (instantiate1)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Except (MonadError)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped)
import Data.Equivalence.Monad (EquivT, classDesc)

import Rules.Infer.Unification
import Rules.Infer.Unification.Offline
import Ast.Type
import Ast.Type.Var
import Ast.Term
import Ast.Term.Var
import Ast.Error.Common
import Context.Term
import Data.Functor.Rec

import Fragment.HM.Ast.Type
import Fragment.HM.Ast.Error
import Fragment.HM.Ast.Term

equivArr :: AsTyHM ty => (Type ty a -> Type ty a -> Bool) -> Type ty a -> Type ty a -> Maybe Bool
equivArr equivFn ty1 ty2 = do
  (p1a, p1b) <- preview _TyArr ty1
  (p2a, p2b) <- preview _TyArr ty2
  return $ equivFn p1a p2a && equivFn p1b p2b

unifyArr :: (UnificationContext e m ty a, AsTyHM ty)
          => ([Type ty a] -> [Type ty a] -> EquivT s (Type ty a) (Type ty a) m ())
          -> UConstraint ty a
          -> Maybe (EquivT s (Type ty a) (Type ty a) m ())
unifyArr unifyMany u = do
  (ty1, ty2) <- preview _UCEq u
  (p1a, p1b) <- preview _TyArr ty1
  (p2a, p2b) <- preview _TyArr ty2
  return $ do
    c1a <- classDesc p1a
    c1b <- classDesc p1b
    c2a <- classDesc p2a
    c2b <- classDesc p2b
    unifyMany [c1a, c1b] [c2a, c2b]

inferTmLam :: (Ord a, AstBound ty pt tm, MonadState s m, HasTyVarSupply s, ToTyVar a, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTyHM ty, AsTmHM ty pt tm, HasTermContext r ty a)
           => (Term ty pt tm a -> m (Type ty a))
           -> Term ty pt tm a
           -> Maybe (m (Type ty a))
inferTmLam inferFn tm = do
  s <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    tyV <- fmap (review _TyVar) freshTyVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyV) $ inferFn tmF
    return $ review _TyArr (tyV, tyRet)

inferTmApp :: (Eq a, EqRec ty, MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsTyHM ty, AsTmHM ty pt tm, AsExpectedTyArr e ty a, AsExpectedEq e ty a)
           => (Term ty pt tm a -> UnifyT ty a m (Type ty a))
           -> Term ty pt tm a
           -> Maybe (UnifyT ty a m (Type ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    tyX <- inferFn tmX
    tyV <- fmap (review _TyVar) freshTyVar
    -- would be nice to tag this with the expectTyArr error somehow
    expectEq tyF (review _TyArr (tyX, tyV))
    return tyV

type HMInferContext e w s r m ty pt tm a = (Ord a, InferContext e w s r m ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ty a, AsTyHM ty, AsExpectedEq e ty a, AsExpectedTyArr e ty a, AsTmHM ty pt tm)

hmInferRules :: HMInferContext e w s r m ty pt tm a
             => InferInput e w s r m ty pt tm a
hmInferRules =
  InferInput
    [ EquivRecurse equivArr ]
    [ UnificationMany unifyArr ]
    [ InferRecurse inferTmLam
    , InferRecurse inferTmApp
    ]
    []
