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
    RulesIn(..)
  , RulesOut(..)
  ) where

import Data.Proxy
import GHC.Exts (Constraint)

import Rules.Unification
import qualified Rules.Kind.Infer.SyntaxDirected as KSD
import qualified Rules.Type.Infer.SyntaxDirected as TSD
import qualified Rules.Type.Infer.Offline as TUO
import Rules.Type
import Rules.Term

import Ast.Kind
import Ast.Type
import Ast.Error
import Ast.Error.Common
import Ast.Warning
import Ast.Pattern
import Ast.Term

class TLAppend (xs :: [k]) (ys :: [k]) where
  type Append xs ys :: [k]

instance TLAppend '[] ys where
  type Append '[] ys = ys

instance TLAppend xs ys => TLAppend (x ': xs) ys where
  type Append (x ': xs) ys = x ': (Append xs ys)

class RulesIn (k :: j) where
  type RuleKindInferSyntaxContext e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a k :: Constraint
  type RuleInferSyntaxContext e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type RuleInferOfflineContext e w s r (m :: * -> *) (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type RuleTypeContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) a k :: Constraint
  type RuleTermContext (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: Constraint
  type KindList k :: [* -> *]
  type TypeList k :: [(* -> *) -> (* -> *) -> * -> *]
  type ErrorList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: [*]
  type WarningList (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: [*]
  type PatternList k :: [(* -> *) -> * -> *]
  type TermList k :: [(* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *]

  inferKindInputSyntax :: RuleKindInferSyntaxContext e w s r m ki ty a k
                       => Proxy k
                       -> KSD.KindRulesInput e w s r m ki ty a
  inferSyntaxInput :: ( RuleInferSyntaxContext e w s r m ki ty pt tm a k
                      , RuleKindInferSyntaxContext e w s r m ki ty a k
                      , RuleTypeContext ki ty a k
                      )
                   => Proxy k
                   -> TSD.InferInput e w s r m ki ty pt tm a
  inferOfflineInput :: RuleInferOfflineContext e w s r m ki ty pt tm a k
                    => Proxy k
                    -> TUO.InferInput e w s r m ki ty pt tm a
  typeInput :: RuleTypeContext ki ty a k => Proxy k -> TypeInput ki ty a
  termInput :: (TermContext ki ty pt tm a, RuleTermContext ki ty pt tm a k) => Proxy k -> TermInput ki ty pt tm a

instance RulesIn '[] where
  type RuleKindInferSyntaxContext e w s r m ki ty a '[] =
    KSD.KindRulesContext e w s r m ki ty a
  type RuleInferSyntaxContext e w s r m ki ty pt tm a '[] =
    TSD.InferContext e w s r m ki ty pt tm a
  type RuleInferOfflineContext e w s r m ki ty pt tm a '[] =
    TUO.InferContext e w s r m ki ty pt tm a
  type RuleTypeContext ki ty a '[] = (() :: Constraint)
  type RuleTermContext ki ty pt tm a '[] = (() :: Constraint)
  type KindList '[] = '[]
  type TypeList '[] = '[]
  type ErrorList ki ty pt tm a '[] = '[ErrUnknownKindError, ErrUnknownTypeError, ErrOccursError (Type ki ty) a, ErrUnificationMismatch (Type ki ty) a, ErrUnificationExpectedEq (Type ki ty) a]
  type WarningList ki ty pt tm a '[] = '[]
  type PatternList '[] = '[]
  type TermList '[] = '[]

  inferKindInputSyntax _ = mempty
  inferSyntaxInput _ = mempty
  inferOfflineInput _ = mempty
  typeInput _ = mempty
  termInput _ = mempty

instance (RulesIn k, RulesIn ks) => RulesIn (k ': ks) where
  type RuleKindInferSyntaxContext e w s r m ki ty a (k ': ks) = (RuleKindInferSyntaxContext e w s r m ki ty a k, RuleKindInferSyntaxContext e w s r m ki ty a ks)
  type RuleInferSyntaxContext e w s r m ki ty pt tm a (k ': ks) = (RuleInferSyntaxContext e w s r m ki ty pt tm a k, RuleInferSyntaxContext e w s r m ki ty pt tm a ks)
  type RuleInferOfflineContext e w s r m ki ty pt tm a (k ': ks) = (RuleInferOfflineContext e w s r m ki ty pt tm a k, RuleInferOfflineContext e w s r m ki ty pt tm a ks)
  type RuleTypeContext ki ty a (k ': ks) = (RuleTypeContext ki ty a k, RuleTypeContext ki ty a ks)
  type RuleTermContext ki ty pt tm a (k ': ks) = (RuleTermContext ki ty pt tm a k, RuleTermContext ki ty pt tm a ks)
  type KindList (k ': ks) = Append (KindList k) (KindList ks)
  type TypeList (k ': ks) = Append (TypeList k) (TypeList ks)
  type ErrorList ki ty pt tm a (k ': ks) = Append (ErrorList ki ty pt tm a k) (ErrorList ki ty pt tm a ks)
  type WarningList ki ty pt tm a (k ': ks) = Append (WarningList ki ty pt tm a k) (WarningList ki ty pt tm a ks)
  type PatternList (k ': ks) = Append (PatternList k) (PatternList ks)
  type TermList (k ': ks) = Append (TermList k) (TermList ks)

  inferKindInputSyntax _ = inferKindInputSyntax (Proxy :: Proxy k) `mappend` inferKindInputSyntax (Proxy :: Proxy ks)
  inferSyntaxInput _ = inferSyntaxInput (Proxy :: Proxy k) `mappend` inferSyntaxInput (Proxy :: Proxy ks)
  inferOfflineInput _ = inferOfflineInput (Proxy :: Proxy k) `mappend` inferOfflineInput (Proxy :: Proxy ks)
  typeInput _ = typeInput (Proxy :: Proxy k) `mappend` typeInput (Proxy :: Proxy ks)
  termInput _ = termInput (Proxy :: Proxy k) `mappend` termInput (Proxy :: Proxy ks)

class RulesOut (k :: j) where

  type RKindF k :: * -> *
  type RTypeF k :: ((* -> *) -> (* -> *) -> * -> *)
  type RPatternF k :: ((* -> *) -> * -> *)
  type RTermF k :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)

  type RType k :: (* -> *)
  type RError (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: *
  type RWarning (ki :: * -> *) (ty :: (* -> *) -> (* -> *) -> * -> *) (pt :: (* -> *) -> * -> *) (tm :: ((* -> *) -> ((* -> *) -> (* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> *)) a k :: *
  type RPattern k :: (* -> *)
  type RTerm k :: (* -> *)

  inferKindOutputSyntax :: ( KSD.KindRulesContext e w s r m ki ty a
                           , RuleKindInferSyntaxContext e w s r m ki ty a k
                           )
                        => Proxy k
                        -> KSD.KindRulesOutput e w s r m ki ty a
  inferSyntaxOutput :: ( TSD.InferContext e w s r m ki ty pt tm a
                       , RuleInferSyntaxContext e w s r m ki ty pt tm a k
                       , KSD.KindRulesContext e w s r m ki ty a
                       , RuleKindInferSyntaxContext e w s r m ki ty a k
                       , RuleTypeContext ki ty a k
                       )
                    => Proxy k
                    -> TSD.InferOutput e w s r m ki ty pt tm a
  inferOfflineOutput :: ( TUO.InferContext e w s r m ki ty pt tm a
                        , RuleInferOfflineContext e w s r m ki ty pt tm a k
                        , RuleTypeContext ki ty a k
                        )
                     => Proxy k
                     -> TUO.InferOutput e w s r m ki ty pt tm a
  typeOutput :: RuleTypeContext ki ty a k
             => Proxy k
             -> TypeOutput ki ty a
  termOutput :: ( TermContext ki ty pt tm a
                , RuleTermContext ki ty pt tm a k
                )
             => Proxy k
             -> TermOutput ki ty pt tm a

instance RulesIn k => RulesOut (k :: j) where

  type RKindF k = KiSum (KindList k)
  type RTypeF k = TySum (TypeList k)
  type RPatternF k = PtSum (PatternList k)
  type RTermF k = TmSum (TermList k)

  type RType k = Type (RKindF k) (RTypeF k)
  type RError ki ty pt tm a k = ErrSum (ErrorList ki ty pt tm a k)
  type RWarning ki ty pt tm a k = WarnSum (WarningList ki ty pt tm a k)
  type RPattern k = Pattern (RPatternF k)
  type RTerm k = Term (RKindF k) (RTypeF k) (RPatternF k) (RTermF k)

  inferKindOutputSyntax =
    KSD.prepareKindRules . inferKindInputSyntax

  inferSyntaxOutput :: forall e w s r m ki ty pt tm a.
                       ( TSD.InferContext e w s r m ki ty pt tm a
                       , RuleInferSyntaxContext e w s r m ki ty pt tm a k
                       , KSD.KindRulesContext e w s r m ki ty a
                       , RuleKindInferSyntaxContext e w s r m ki ty a k
                       , RuleTypeContext ki ty a k
                       )
                    => Proxy k
                    -> TSD.InferOutput e w s r m ki ty pt tm a
  inferSyntaxOutput p =
     let
       ikos :: ( KSD.KindRulesContext e w s r m ki ty a
               , RuleKindInferSyntaxContext e w s r m ki ty a k
               )
            => KSD.KindRulesOutput e w s r m ki ty a
       ikos = inferKindOutputSyntax p
       inferKind = KSD.kroInfer ikos
       normalize = toNormalizeType $ typeOutput p
     in
       TSD.prepareInfer inferKind normalize .
       inferSyntaxInput $ p
  inferOfflineOutput p =
    TUO.prepareInfer (toNormalizeType $ typeOutput p) .
    inferOfflineInput $ p
  typeOutput =
    prepareType . typeInput
  termOutput =
    prepareTerm . termInput
