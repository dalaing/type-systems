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

equivArr :: AsTyHM ki ty => (Type ki ty a -> Type ki ty a -> Bool) -> Type ki ty a -> Type ki ty a -> Maybe Bool
equivArr equivFn ty1 ty2 = do
  (p1a, p1b) <- preview _TyArr ty1
  (p2a, p2b) <- preview _TyArr ty2
  return $ equivFn p1a p2a && equivFn p1b p2b

unifyArr :: (UnificationContext e m ki ty a, AsTyHM ki ty)
          => ([Type ki ty a] -> [Type ki ty a] -> EquivT s (Type ki ty a) (Type ki ty a) m ())
          -> UConstraint ki ty a
          -> Maybe (EquivT s (Type ki ty a) (Type ki ty a) m ())
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

inferTmLam :: (Ord a, AstBound ki ty pt tm, MonadState s m, HasTyVarSupply s, ToTyVar a, HasTmVarSupply s, ToTmVar a, MonadReader r m, AsTyHM ki ty, AsTmHM ki ty pt tm, HasTermContext r ki ty a)
           => (Term ki ty pt tm a -> m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (m (Type ki ty a))
inferTmLam inferFn tm = do
  s <- preview _TmLam tm
  return $ do
    v <- freshTmVar
    tyV <- fmap (review _TyVar) freshTyVar
    let tmF = review _Wrapped $ instantiate1 (review (_AVar . _ATmVar) v) s
    tyRet <- local (termContext %~ insertTerm v tyV) $ inferFn tmF
    return $ review _TyArr (tyV, tyRet)

inferTmApp :: (Eq a, EqRec (ty ki), MonadState s m, HasTyVarSupply s, ToTyVar a, MonadError e m, AsTyHM ki ty, AsTmHM ki ty pt tm, AsExpectedTyArr e ki ty a, AsExpectedEq e ki ty a)
           => (Term ki ty pt tm a -> UnifyT ki ty a m (Type ki ty a))
           -> Term ki ty pt tm a
           -> Maybe (UnifyT ki ty a m (Type ki ty a))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    tyX <- inferFn tmX
    tyV <- fmap (review _TyVar) freshTyVar
    -- would be nice to tag this with the expectTyArr error somehow
    expectEq tyF (review _TyArr (tyX, tyV))
    return tyV

type HMInferContext e w s r m ki ty pt tm a = (Ord a, InferContext e w s r m ki ty pt tm a, MonadState s m, HasTyVarSupply s, ToTyVar a, HasTmVarSupply s, ToTmVar a, MonadReader r m, HasTermContext r ki ty a, AsTyHM ki ty, AsExpectedEq e ki ty a, AsExpectedTyArr e ki ty a, AsTmHM ki ty pt tm)

hmInferRules :: HMInferContext e w s r m ki ty pt tm a
             => InferInput e w s r m ki ty pt tm a
hmInferRules =
  InferInput
    [ EquivRecurse equivArr ]
    [ UnificationMany unifyArr ]
    [ InferRecurse inferTmLam
    , InferRecurse inferTmApp
    ]
    []
