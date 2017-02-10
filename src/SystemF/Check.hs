{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- only for the examples
{-# LANGUAGE OverloadedStrings #-}
module SystemF.Check where
{-
  (
    check
  ) where
-}

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Control.Monad (unless)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Error.Lens

import SystemF.Scratch2

data TermContext ty tmV tyV = TermContext (M.Map tmV (ty tyV))

emptyTermContext :: TermContext ty tmV tyV
emptyTermContext = TermContext M.empty

class HasTermContext l ty tmV tyV | l -> ty, l -> tmV, l -> tyV where
  termContext :: Lens' l (TermContext ty tmV tyV)

instance HasTermContext (TermContext ty tmV tyV) ty tmV tyV where
  termContext = id

class AsUnboundTermVariable e tm | e -> tm where
  _UnboundTermVariable :: Prism' e tm

lookupTerm :: (Ord tmV, MonadReader r m, MonadError e m, HasTermContext r ty tmV tyV, AsUnboundTermVariable e tmV) => tmV -> m (ty tyV)
lookupTerm v = do
  TermContext m <- view termContext
  case M.lookup v m of
    Nothing -> throwing _UnboundTermVariable v
    Just ty -> return ty

insertTerm :: Ord tmV => tmV -> ty tyV -> TermContext ty tmV tyV -> TermContext ty tmV tyV
insertTerm v ty (TermContext m) = TermContext (M.insert v ty m)

class HasTmVarSupply s where
  tmVarSupply :: Lens' s Int

class ToTmVar a where
  toTmVar :: Int -> a

instance ToTmVar T.Text where
  toTmVar x = T.append "x" (T.pack . show $ x)

freshTmVar :: (MonadState s m, HasTmVarSupply s, ToTmVar a) => m a
freshTmVar = do
  x <- use tmVarSupply
  tmVarSupply %= succ
  return $ toTmVar x

class HasTyVarSupply s where
  tyVarSupply :: Lens' s Int

class ToTyVar a where
  toTyVar :: Int -> a

instance ToTyVar T.Text where
  toTyVar x = T.append "X" (T.pack . show $ x)

freshTyVar :: (MonadState s m, HasTyVarSupply s, ToTyVar a) => m a
freshTyVar = do
  x <- use tyVarSupply
  tyVarSupply %= succ
  return $ toTyVar x

class AsUnexpected e ty | e -> ty where
  _Unexpected :: Prism' e (ty, ty)

expect :: (Eq ty, MonadError e m, AsUnexpected e ty) => ty -> ty -> m ()
expect ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _Unexpected (ty1, ty2)

class AsExpectedEq e ty | e -> ty where
  _ExpectedEq :: Prism' e (ty, ty)

expectEq :: (Eq ty, MonadError e m, AsExpectedEq e ty) => ty -> ty -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

class AsExpectedTyArr e ty | e -> ty where
  _ExpectedTyArr :: Prism' e ty

expectTyArr :: (MonadError e m, AsExpectedTyArr e (ty T.Text), AsType ty) => ty T.Text -> m (ty T.Text, ty T.Text)
expectTyArr ty =
  case preview _TyArr ty of
    Just (tyArg, tyRet) -> return (tyArg, tyRet)
    _ -> throwing _ExpectedTyArr ty

class AsExpectedTyAll e ty | e -> ty where
  _ExpectedTyAll :: Prism' e ty

expectTyAll :: (Eq a, MonadError e m, MonadState s m, HasTyVarSupply s, ToTyVar a, AsExpectedTyAll e (ty a), AsType ty) => ty a -> m (a, ty a)
expectTyAll ty = do
  x <- freshTyVar
  case preview _TyAll (x, ty) of
    Just (v, tyA) -> return (v, tyA)
    _ -> throwing _ExpectedTyAll ty

-- type AsSimpleSTLCErrors e tm ty = (Ord tm, Eq ty, AsUnboundTermVariable e tm, AsUnboundTypeVariable e ty, AsUnexpected e ty, AsExpectedEq e ty, AsExpectedFnTy e ty, AsExpectedFnTm e tm)

-- check :: (MonadReader r m, MonadError e m, AsType ty, AsTerm ty tm) => tm a -> ty a -> m ()
-- check = _

{-
check (TmLam v tyL s) ty = do
  let Just tyL' = closed tyL
  (tyArg, tyRet) <- expectFnTy ty
  expectEq tyL' tyArg
  local (termContext %~ insertTerm v tyArg) $ check (instantiate1 (TmVar v) s) tyRet
-}

class AsUnknownTypeError e where
  _UnknownTypeError :: Prism' e ()

data Errors =
    EUnboundTermVariable T.Text
  | EUnexpected (Type T.Text) (Type T.Text)
  | EExpectedEq (Type T.Text) (Type T.Text)
  | EExpectedTyArr (Type T.Text)
  | EExpectedTyAll (Type T.Text)
  | EUnknownTypeError
  deriving (Eq, Ord, Show)

makePrisms ''Errors

instance AsUnboundTermVariable Errors T.Text where
  _UnboundTermVariable = _EUnboundTermVariable

instance AsUnexpected Errors (Type T.Text) where
  _Unexpected = _EUnexpected

instance AsExpectedEq Errors (Type T.Text) where
  _ExpectedEq = _EExpectedEq

instance AsExpectedTyArr Errors (Type T.Text) where
  _ExpectedTyArr = _EExpectedTyArr

instance AsExpectedTyAll Errors (Type T.Text) where
  _ExpectedTyAll = _EExpectedTyAll

instance AsUnknownTypeError Errors where
  _UnknownTypeError = _EUnknownTypeError

data Supplies = Supplies {
    _sTyVar :: Int
  , _sTmVar :: Int
  }

makeLenses ''Supplies

instance HasTyVarSupply Supplies where
  tyVarSupply = sTyVar

instance HasTmVarSupply Supplies where
  tmVarSupply = sTmVar

initialSupplies :: Supplies
initialSupplies = Supplies 0 0

check' :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a)) => (tm a -> m (ty a)) -> tm a -> ty a -> m ()
check' inferFn tm ty = do
  tyAc <- inferFn tm
  expect ty tyAc

runCheck :: Term Type T.Text -> Type T.Text -> Either Errors ()
runCheck tm ty = runExcept . flip evalStateT initialSupplies . flip runReaderT emptyTermContext $ check tm ty

check :: (Eq (ty T.Text), MonadState s m, HasTyVarSupply s, MonadReader r m, MonadError e m, AsType ty, AsTerm ty tm, HasTermContext r ty T.Text T.Text, AsUnknownTypeError e, AsUnboundTermVariable e T.Text, AsUnexpected e (ty T.Text), AsExpectedEq e (ty T.Text), AsExpectedTyArr e (ty T.Text), AsExpectedTyAll e (ty T.Text)) => tm T.Text -> ty T.Text -> m ()
check = check' infer

runInfer :: Term Type T.Text -> Either Errors (Type T.Text)
runInfer tm = runExcept . flip evalStateT initialSupplies . flip runReaderT emptyTermContext $ infer tm

infer :: (Eq (ty T.Text), MonadState s m, HasTyVarSupply s, MonadReader r m, MonadError e m, AsType ty, AsTerm ty tm, HasTermContext r ty T.Text T.Text, AsUnknownTypeError e, AsUnboundTermVariable e T.Text, AsUnexpected e (ty T.Text), AsExpectedEq e (ty T.Text), AsExpectedTyArr e (ty T.Text), AsExpectedTyAll e (ty T.Text)) => tm T.Text -> m (ty T.Text)
infer tm = do
  tyV <- freshTyVar
  fromMaybe (throwing _UnknownTypeError ()) .
    asum .
    fmap ($ tm) $ [
      inferTmVar
    , inferTmLam infer
    , inferTmApp infer
    , inferTmLamTy infer tyV
    , inferTmAppTy infer
    , inferTmAdd infer
    , inferTmInt
    ]

inferTmVar :: (Ord a, MonadReader r m, MonadError e m, AsTerm ty tm, HasTermContext r ty a a, AsUnboundTermVariable e a) => tm a -> Maybe (m (ty a))
inferTmVar tm = do
  v <- preview _TmVar tm
  return $ lookupTerm v

inferTmLam :: (MonadReader r m, AsType ty, AsTerm ty tm, HasTermContext r ty T.Text T.Text) => (tm T.Text -> m (ty T.Text)) -> tm T.Text -> Maybe (m (ty T.Text))
inferTmLam inferFn tm = do
  (v, tyArg, tmF) <- preview _TmLam tm
  return $ do
    tyRet <- local (termContext %~ insertTerm v tyArg) $ inferFn tmF
    return $ review _TyArr (tyArg, tyRet)

inferTmApp :: (Eq (ty T.Text), MonadError e m, AsType ty, AsTerm ty tm, AsExpectedTyArr e (ty T.Text), AsExpectedEq e (ty T.Text)) => (tm T.Text -> m (ty T.Text)) -> tm T.Text -> Maybe (m (ty T.Text))
inferTmApp inferFn tm = do
  (tmF, tmX) <- preview _TmApp tm
  return $ do
    tyF <- inferFn tmF
    (tyArg, tyRet) <- expectTyArr tyF
    tyX <- inferFn tmX
    expectEq tyArg tyX
    return tyRet

inferTmLamTy :: (Eq a, MonadState s m, HasTyVarSupply s, ToTyVar a, AsType ty, AsTerm ty tm) => (tm a -> m (ty a)) -> a -> tm a -> Maybe (m (ty a))
inferTmLamTy inferFn v tm = do
  (_, tmF) <- preview _TmLamTy (v, tm)
  return $ do
    tyA <- inferFn tmF
    return . snd $ review _TyAll (v, tyA)

inferTmAppTy :: (Eq a, MonadError e m, MonadState s m, HasTyVarSupply s, ToTyVar a, AsType ty, AsTerm ty tm, AsExpectedTyAll e (ty a)) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmAppTy inferFn tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  return $ do
    tyF <- inferFn tmF
    (v, tyA) <- expectTyAll tyF
    let Just ty = substTyAll tyX tyF
    return ty

inferTmAdd :: (Eq (ty a), MonadError e m, AsUnexpected e (ty a), AsType ty, AsTerm ty tm) => (tm a -> m (ty a)) -> tm a -> Maybe (m (ty a))
inferTmAdd inferFn tm = do
  (tm1, tm2) <- preview _TmAdd tm
  return $ do
    let ty = review _TyInt ()
    check' inferFn tm1 ty
    check' inferFn tm2 ty
    return ty

inferTmInt :: (Monad m, AsType ty, AsTerm ty tm) => tm a -> Maybe (m (ty a))
inferTmInt tm = do
  _ <- preview _TmInt tm
  return . return $ review _TyInt ()

tmFst :: Term Type T.Text
tmFst = snd . tmLamTy "X" . snd . tmLamTy "Y" . tmLam "x" (tyVar "X") . tmLam "y" (tyVar "Y") $ tmVar "x"

tyFst :: Type T.Text
tyFst = snd . tyAll "X" . snd . tyAll "Y" $ tyArr (tyVar "X") (tyArr (tyVar "Y") (tyVar "X"))

tmFstA :: Term Type T.Text
tmFstA = snd . tmLamTy "A" . snd . tmLamTy "B" . tmLam "a" (tyVar "A") . tmLam "b" (tyVar "B") $ tmVar "a"

tyFstA :: Type T.Text
tyFstA = snd . tyAll "A" . snd . tyAll "B" $ tyArr (tyVar "A") (tyArr (tyVar "B") (tyVar "A"))

tmFst2 :: Term Type T.Text
tmFst2 = tmAppTy tmFst tyInt

tyFst2 :: Type T.Text
tyFst2 = snd . tyAll "B" $ tyArr tyInt (tyArr (tyVar "B") tyInt)

tmFst3 :: Term Type T.Text
tmFst3 = tmAppTy tmFst2 (tyArr tyInt tyInt)

tyFst3 :: Type T.Text
tyFst3 = tyArr tyInt (tyArr (tyArr tyInt tyInt) tyInt)

