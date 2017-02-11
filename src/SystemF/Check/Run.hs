{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- only for the examples
{-# LANGUAGE OverloadedStrings #-}
module SystemF.Check.Run (
    runCheck
  , runInfer
  ) where

import qualified Data.Text as T

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)

import Control.Lens

import SystemF.Type
import SystemF.Term
import SystemF.Check.Classes
import SystemF.Check

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

runCheck :: Term Type T.Text -> Type T.Text -> Either Errors ()
runCheck tm ty = runExcept . flip evalStateT initialSupplies . flip runReaderT emptyTermContext $ check tm ty

runInfer :: Term Type T.Text -> Either Errors (Type T.Text)
runInfer tm = runExcept . flip evalStateT initialSupplies . flip runReaderT emptyTermContext $ infer tm

{-
tmFst :: Term Type T.Text
tmFst = tmLamTy "X" . tmLamTy "Y" . tmLam "x" (tyVar "X") . tmLam "y" (tyVar "Y") $ tmVar "x"

tyFst :: Type T.Text
tyFst = tyAll "X" . tyAll "Y" $ tyArr (tyVar "X") (tyArr (tyVar "Y") (tyVar "X"))

tmFstA :: Term Type T.Text
tmFstA = tmLamTy "A" . tmLamTy "B" . tmLam "a" (tyVar "A") . tmLam "b" (tyVar "B") $ tmVar "a"

tyFstA :: Type T.Text
tyFstA = tyAll "A" . tyAll "B" $ tyArr (tyVar "A") (tyArr (tyVar "B") (tyVar "A"))

tmFst2 :: Term Type T.Text
tmFst2 = tmAppTy tmFst tyInt

tyFst2 :: Type T.Text
tyFst2 = tyAll "B" $ tyArr tyInt (tyArr (tyVar "B") tyInt)

tmFst3 :: Term Type T.Text
tmFst3 = tmAppTy tmFst2 (tyArr tyInt tyInt)

tyFst3 :: Type T.Text
tyFst3 = tyArr tyInt (tyArr (tyArr tyInt tyInt) tyInt)
-}
