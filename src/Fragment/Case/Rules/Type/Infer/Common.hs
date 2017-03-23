{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fragment.Case.Rules.Type.Infer.Common (
    CaseInferTypeConstraint
  , caseInferTypeInput
  ) where

import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.Proxy (Proxy(..))

import Bound (instantiate)
import Bound.Scope (bindings)
import Control.Lens (review, preview, (%~))
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty)

import Ast.Type
import Ast.Pattern
import Ast.Term
import Ast.Term.Var
import Context.Term
import Data.Functor.Rec

import Fragment.Case.Ast.Error
import Fragment.Case.Ast.Warning
import Fragment.Case.Ast.Term

import Rules.Type.Infer.Common
import Rules.Type.Infer.SyntaxDirected (ISyntax)
import Rules.Type.Infer.Offline (IOffline)

class MkInferType i => CaseInferTypeHelper i where
  liftInfer :: Monad m
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> m x
            -> InferTypeMonad ki ty a m i x

instance CaseInferTypeHelper ISyntax where
  liftInfer _ _ _ _ _ = id

instance CaseInferTypeHelper IOffline where
  liftInfer _ _ _ _ _ = lift

type CaseInferTypeConstraint e w s r m ki ty pt tm a i =
  ( BasicInferTypeConstraint e w s r m ki ty pt tm a i
  , Ord a
  , EqRec (ty ki)
  , AstBound ki ty pt tm
  , MonadState s (InferTypeMonad ki ty a m i)
  , HasTmVarSupply s
  , ToTmVar a
  , MonadReader r (InferTypeMonad ki ty a m i)
  , HasTermContext r ki ty a
  , MonadWriter [w] m
  , AsUnusedPatternVariables w a
  , AsShadowingPatternVariables w a
  , AsTmCase ki ty pt tm
  , MonadError e (InferTypeMonad ki ty a m i)
  , AsDuplicatedPatternVariables e a
  , AsExpectedPattern e ki ty pt tm a
  , CaseInferTypeHelper i
  )

inferTmCase :: CaseInferTypeConstraint e w s r m ki ty pt tm a i
            => Proxy (MonadProxy e w s r m)
            -> Proxy ki
            -> Proxy ty
            -> Proxy a
            -> Proxy i
            -> (Term ki ty pt tm a -> InferTypeMonad ki ty a m i (Type ki ty a))
            -> (Pattern pt a -> Type ki ty a -> InferTypeMonad ki ty a m i [Type ki ty a])
            -> Term ki ty pt tm a
            -> Maybe (InferTypeMonad ki ty a m i (Type ki ty a))
inferTmCase m pki pty pa i inferFn checkFn tm = do
  (tmC, alts) <- preview _TmCase tm
  return $ do
    let
      go ty (Alt p s) = do
          p' <- expectPattern p

          let vp = toList p'
          checkForDuplicatedPatternVariables vp
          let scopeBindings = bindings s
          liftInfer m pki pty pa i $ checkForUnusedPatternVariables vp scopeBindings
          contextBindings <- lookupTermBindings
          -- this won't fire at the moment, because bound is taking care of shadowing for us
          -- we'll need to track a bit more info about the original form of the AST before this works
          liftInfer m pki pty pa i $ checkForShadowingPatternVariables vp contextBindings

          vs <- replicateM (length p') freshTmVar
          tys <- checkFn p' ty
          let setup = foldr (.) id (zipWith insertTerm vs tys)

          let tm' = review _Wrapped .
                    instantiate (review (_Unwrapped . _TmVar) . (vs !!)) $
                    s
          local (termContext %~ setup) $ inferFn tm'
    tyC <- inferFn tmC
    tys <- mapM (go tyC) alts
    expectTypeAllEq m i tys

caseInferTypeInput :: CaseInferTypeConstraint e w s r m ki ty pt tm a i
                   => Proxy (MonadProxy e w s r m)
                   -> Proxy i
                   -> InferTypeInput e w s r m (InferTypeMonad ki ty a m i) ki ty pt tm a
caseInferTypeInput m i =
  InferTypeInput [] [ InferTypePCheck $ inferTmCase m (Proxy :: Proxy ki) (Proxy :: Proxy ty) (Proxy :: Proxy a) i ] []
