{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Rules (
    AstIn(..)
  , AstOut(..)
  , RulesIn(..)
  , RulesOut(..)
  ) where

import Data.Proxy
import GHC.Exts (Constraint)

import Control.Monad.Trans (lift)

import Rules.Unification
import qualified Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Rules.Type.Infer.SyntaxDirected as TSD
import qualified Rules.Type.Infer.Offline as TUO
import Rules.Type
import Rules.Term
import Util.TypeList

import Ast.Kind
import Ast.Type
import Ast.Error
import Ast.Error.Common
import Ast.Warning
import Ast.Pattern
import Ast.Term

class AstIn (k :: j) where
  type KindList k :: [* -> *]
  type TypeList k :: [(* -> *) -> (* -> *) -> * -> *]
  type PatternList k :: [(* -> *) -> * -> *]
  type TermList k :: [(* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *]

instance AstIn '[] where
  type KindList '[] = '[]
  type TypeList '[] = '[]
  type PatternList '[] = '[]
  type TermList '[] = '[]

instance (AstIn k, AstIn ks) => AstIn (k ': ks) where
  type KindList (k ': ks) = Append (KindList k) (KindList ks)
  type TypeList (k ': ks) = Append (TypeList k) (TypeList ks)
  type PatternList (k ': ks) = Append (PatternList k) (PatternList ks)
  type TermList (k ': ks) = Append (TermList k) (TermList ks)

class AstOut (k :: j) where

  type RKindF k :: * -> *
  type RTypeF k :: ((* -> *) -> (* -> *) -> * -> *)
  type RPatternF k :: ((* -> *) -> * -> *)
  type RTermF k :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)

  type RKind k :: *
  type RType k :: (* -> *)
  type RPattern k :: (* -> *)
  type RTerm k :: (* -> *)

instance AstIn k => AstOut (k :: j) where
  type RKindF k = KiSum (KindList k)
  type RTypeF k = TySum (TypeList k)
  type RPatternF k = PtSum (PatternList k)
  type RTermF k = TmSum (TermList k)

  type RKind k = Kind (RKindF k)
  type RType k = Type (RKindF k) (RTypeF k)
  type RPattern k = Pattern (RPatternF k)
  type RTerm k = Term (RKindF k) (RTypeF k) (RPatternF k) (RTermF k)

class AstIn k => RulesIn (k :: j) where
  type InferKindContextSyntax e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a k :: Constraint
  type InferTypeContextSyntax e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type InferTypeContextOffline e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type RuleTypeContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a k :: Constraint
  type RuleTermContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint

  type ErrorList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: [*]
  type WarningList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: [*]

  inferKindInputSyntax :: InferKindContextSyntax e w s r m ki ty a k
                       => Proxy k
                       -> KSD.InferKindInput e w s r m ki ty a
  inferTypeInputSyntax :: ( InferTypeContextSyntax e w s r m ki ty pt tm a k
                      , InferKindContextSyntax e w s r m ki ty a k
                      , RuleTypeContext ki ty a k
                      )
                   => Proxy k
                   -> TSD.InferTypeInput e w s r m m ki ty pt tm a
  inferTypeInputOffline :: InferTypeContextOffline e w s r m ki ty pt tm a k
                    => Proxy k
                    -> TUO.InferTypeInput e w s r m (TUO.UnifyT ki ty a m) ki ty pt tm a
  typeInput :: RuleTypeContext ki ty a k => Proxy k -> TypeInput ki ty a
  termInput :: (TermContext ki ty pt tm a, RuleTermContext ki ty pt tm a k) => Proxy k -> TermInput ki ty pt tm a

instance RulesIn '[] where
  type InferKindContextSyntax e w s r m ki ty a '[] =
    KSD.InferKindContext e w s r m ki ty a
  type InferTypeContextSyntax e w s r m ki ty pt tm a '[] =
    TSD.InferTypeContext e w s r m ki ty pt tm a
  type InferTypeContextOffline e w s r m ki ty pt tm a '[] =
    TUO.InferTypeContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a '[] = (() :: Constraint)
  type RuleTermContext ki ty pt tm a '[] = (() :: Constraint)
  type ErrorList ki ty pt tm a '[] = '[ErrUnknownKindError, ErrUnknownTypeError, ErrOccursError (Type ki ty) a, ErrUnificationMismatch (Type ki ty) a, ErrUnificationExpectedEq (Type ki ty) a]
  type WarningList ki ty pt tm a '[] = '[]

  inferKindInputSyntax _ = mempty
  inferTypeInputSyntax _ = mempty
  inferTypeInputOffline _ = mempty
  typeInput _ = mempty
  termInput _ = mempty

instance (RulesIn k, RulesIn ks) => RulesIn (k ': ks) where
  type InferKindContextSyntax e w s r m ki ty a (k ': ks) = (InferKindContextSyntax e w s r m ki ty a k, InferKindContextSyntax e w s r m ki ty a ks)
  type InferTypeContextSyntax e w s r m ki ty pt tm a (k ': ks) = (InferTypeContextSyntax e w s r m ki ty pt tm a k, InferTypeContextSyntax e w s r m ki ty pt tm a ks)
  type InferTypeContextOffline e w s r m ki ty pt tm a (k ': ks) = (InferTypeContextOffline e w s r m ki ty pt tm a k, InferTypeContextOffline e w s r m ki ty pt tm a ks)
  type RuleTypeContext ki ty a (k ': ks) = (RuleTypeContext ki ty a k, RuleTypeContext ki ty a ks)
  type RuleTermContext ki ty pt tm a (k ': ks) = (RuleTermContext ki ty pt tm a k, RuleTermContext ki ty pt tm a ks)
  type ErrorList ki ty pt tm a (k ': ks) = Append (ErrorList ki ty pt tm a k) (ErrorList ki ty pt tm a ks)
  type WarningList ki ty pt tm a (k ': ks) = Append (WarningList ki ty pt tm a k) (WarningList ki ty pt tm a ks)

  inferKindInputSyntax _ = inferKindInputSyntax (Proxy :: Proxy k) `mappend` inferKindInputSyntax (Proxy :: Proxy ks)
  inferTypeInputSyntax _ = inferTypeInputSyntax (Proxy :: Proxy k) `mappend` inferTypeInputSyntax (Proxy :: Proxy ks)
  inferTypeInputOffline _ = inferTypeInputOffline (Proxy :: Proxy k) `mappend` inferTypeInputOffline (Proxy :: Proxy ks)
  typeInput _ = typeInput (Proxy :: Proxy k) `mappend` typeInput (Proxy :: Proxy ks)
  termInput _ = termInput (Proxy :: Proxy k) `mappend` termInput (Proxy :: Proxy ks)

class AstOut k => RulesOut (k :: j) where

  type RError (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: *
  type RWarning (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: *

  inferKindOutputSyntax :: ( KSD.InferKindContext e w s r m ki ty a
                           , InferKindContextSyntax e w s r m ki ty a k
                           )
                        => Proxy k
                        -> KSD.InferKindOutput e w s r m ki ty a
  inferTypeOutputSyntax :: ( TSD.InferTypeContext e w s r m ki ty pt tm a
                       , InferTypeContextSyntax e w s r m ki ty pt tm a k
                       , KSD.InferKindContext e w s r m ki ty a
                       , InferKindContextSyntax e w s r m ki ty a k
                       , RuleTypeContext ki ty a k
                       )
                    => Proxy k
                    -> TSD.InferTypeOutput e w s r m ki ty pt tm a
  inferTypeOutputOffline :: ( TUO.InferTypeContext e w s r m ki ty pt tm a
                        , InferTypeContextOffline e w s r m ki ty pt tm a k
                        , KSD.InferKindContext e w s r m ki ty a
                        , InferKindContextSyntax e w s r m ki ty a k
                        , RuleTypeContext ki ty a k
                        )
                     => Proxy k
                     -> TUO.InferTypeOutput e w s r m ki ty pt tm a
  typeOutput :: RuleTypeContext ki ty a k
             => Proxy k
             -> TypeOutput ki ty a
  termOutput :: ( TermContext ki ty pt tm a
                , RuleTermContext ki ty pt tm a k
                )
             => Proxy k
             -> TermOutput ki ty pt tm a

instance RulesIn k => RulesOut (k :: j) where

  type RError ki ty pt tm a k = ErrSum (ErrorList ki ty pt tm a k)
  type RWarning ki ty pt tm a k = WarnSum (WarningList ki ty pt tm a k)

  inferKindOutputSyntax =
    KSD.prepareInferKind . inferKindInputSyntax

  inferTypeOutputSyntax :: forall e w s r m ki ty pt tm a.
                       ( TSD.InferTypeContext e w s r m ki ty pt tm a
                       , InferTypeContextSyntax e w s r m ki ty pt tm a k
                       , KSD.InferKindContext e w s r m ki ty a
                       , InferKindContextSyntax e w s r m ki ty a k
                       , RuleTypeContext ki ty a k
                       )
                    => Proxy k
                    -> TSD.InferTypeOutput e w s r m ki ty pt tm a
  inferTypeOutputSyntax p =
     let
       ikos :: ( KSD.InferKindContext e w s r m ki ty a
               , InferKindContextSyntax e w s r m ki ty a k
               )
            => KSD.InferKindOutput e w s r m ki ty a
       ikos = inferKindOutputSyntax p
       inferKind = KSD.kroInfer ikos
       normalize = toNormalizeType $ typeOutput p
     in
       TSD.prepareInferType inferKind normalize .
       inferTypeInputSyntax $ p

  inferTypeOutputOffline :: forall e w s r m ki ty pt tm a.
                       ( TUO.InferTypeContext e w s r m ki ty pt tm a
                       , InferTypeContextOffline e w s r m ki ty pt tm a k
                       , KSD.InferKindContext e w s r m ki ty a
                       , InferKindContextSyntax e w s r m ki ty a k
                       , RuleTypeContext ki ty a k
                       )
                    => Proxy k
                    -> TUO.InferTypeOutput e w s r m ki ty pt tm a
  inferTypeOutputOffline p =
     let
       ikos :: ( KSD.InferKindContext e w s r m ki ty a
               , InferKindContextSyntax e w s r m ki ty a k
               )
            => KSD.InferKindOutput e w s r m ki ty a
       ikos = inferKindOutputSyntax p
       inferKind = KSD.kroInfer ikos
       normalize = toNormalizeType $ typeOutput p
     in
       TUO.prepareInferType (lift . inferKind) normalize .
       inferTypeInputOffline $ p

  typeOutput =
    prepareType . typeInput
  termOutput =
    prepareTerm . termInput
