{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
module Fragment.SystemF.Rules.Term (
    SystemFEvalConstraint
  , systemFEvalRules
  ) where

import Bound (Bound, instantiate1)
import Control.Lens (review, preview)
import Control.Lens.Wrapped (_Wrapped)

import Rules.Term
import Ast.Term

import Fragment.SystemF.Ast.Term

valTmLamTy :: AsTmSystemF ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
valTmLamTy tm = do
  _ <- preview _TmLamTy tm
  return tm

stepTmAppTy1 :: AsTmSystemF ki ty pt tm => (Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)) -> Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmAppTy1 evalFn tm = do
  (f, x) <- preview _TmAppTy tm
  f' <- evalFn f
  return $ review _TmAppTy (f', x)

stepTmLamTyAppTy :: (Bound ki, Bound (ty ki), Bound pt, Bound (tm ki ty pt)) => AsTmSystemF ki ty pt tm => Term ki ty pt tm a -> Maybe (Term ki ty pt tm a)
stepTmLamTyAppTy tm = do
  (tmF, tyX) <- preview _TmAppTy tm
  (_, s) <- preview _TmLamTy tmF
  return . review _Wrapped . instantiate1 (review _TmType tyX) $ s

type SystemFEvalConstraint ki ty pt tm a =
  AsTmSystemF ki ty pt tm

systemFEvalRules :: SystemFEvalConstraint ki ty pt tm a
                 => EvalInput ki ty pt tm a
systemFEvalRules =
  EvalInput
  [ ValueBase valTmLamTy ]
  [ StepRecurse stepTmAppTy1
  , StepBase stepTmLamTyAppTy
  ]
  []
