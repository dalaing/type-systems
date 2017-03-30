{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Fragment.Tuple.Ast.Type (
    TyFTuple
  , AsTyTuple(..)
  ) where

import Data.Functor.Classes (showsUnaryWith)

import Bound (Bound(..))
import Control.Lens.Iso (mapping)
import Control.Lens.Prism (Prism')
import Control.Lens.Wrapped (_Wrapped, _Unwrapped)
import Control.Lens.TH (makePrisms)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)

import Ast.Type
import Data.Bitransversable
import Data.Functor.Rec

data TyFTuple (ki :: (* -> *) -> * -> *) f a =
  --  TyTupleEmptyF
  -- | TyTupleHasF Int (f a) (TyFTuple f a)
  TyTupleF [f a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- TODO need to make a prism between a list of types and a complete tuple
-- use the prism in show so that we either get the usual tuple presentation if complete
-- or a constraint-like presentation if incomplete
--
-- will need ptTupleIndexed, ptTupleComplete, ptTupleIncomplete
--
-- the term can probably have ix replaced with get and set
-- duplicates can be replaced as we go? either that or we add a drop function as well
-- - if we're adding a `drop`, we should consider `add`
-- - tyEquiv with duplicates would be an in order comparison (after a stable sort) of the first occurences
-- we can then have something like f :: (a -> b) - > Has 2 a c -> Has 2 b c
-- need to be able to deal with constexprs before we can abstract the int out a bit further
--
-- need a helper to normalize the tuples before they go through eq / ord / show
-- this is probably an prism that goes to/from a list
-- - go via a list of (Int, Type) pairs
-- - prism rules out duplicates
-- could be used by the complete prism
-- - the completeness prism rules out gaps
--
-- use the prism in the tyTuple helper
-- add the tyTupleHas helper as well
--
-- if we can do this, we can probably do records and variants
-- probably need to add ascription to get variants working in the syntax directed version?
-- - wait and see how tuples fair before we make that call

-- unification notes

  -- | UCEqAt (Type ty a) Int (Type ty a) -- ty ix tuple
  -- rhs is tuple, lhs is var -- equate var with the type at the index in the tuple
  -- rhs is tuple, lhs is type -- unify the lhs type and the type at the index in the tuple
  -- rhs is var, what do we do?
  -- - we can defer it until the rhs is not a var
  -- - what if we have UCEqAt x 1 y and UCEqAt z 1 y
  --   - we should be able to unify x and z
  --   - we can do that if we use a type like Has 1 x y => y
  --   - what if we have multiple constraints?
  --     - (Has 1 x y, Has 2 z y) => y
  --     - (Has 1 x y, Has 2 z y, Eq z) => y
  --     - can we go for Has 0 x (Has 1 z y) as the type
  --       - can possibly replace the list based type with this
  --       - needs kind checking, to make sure that y has kind tuple (either var or concrete)
  --     - would need a subsumption rule
  --       - Has 1 Int d < Has 0 a (Has 1 Int (Has 2 c Empty))
  --         - so we can do type checking for partial types
  --         - also probably used in inferecne
  --           - tmTupleIx (tmTuple [tmInt 1]) 2
  --       - do we need to be able to compute d = Has 0 a (Has 2 c Empty)?
  --       - does that come out in the unfication and flow to everywhere else?


makePrisms ''TyFTuple

deriveEq1 ''TyFTuple
deriveOrd1 ''TyFTuple
deriveShow1 ''TyFTuple

instance EqRec (TyFTuple ki) where
  liftEqRec eR _ (TyTupleF xs) (TyTupleF ys) = and $ zipWith eR xs ys

instance OrdRec (TyFTuple ki) where
  liftCompareRec _ _ (TyTupleF []) (TyTupleF []) = EQ
  liftCompareRec _ _ (TyTupleF []) (TyTupleF (_ : _)) = LT
  liftCompareRec _ _ (TyTupleF (_ : _)) (TyTupleF []) = GT
  liftCompareRec cR c (TyTupleF (x : xs)) (TyTupleF (y : ys)) =
    case cR x y of
      EQ -> liftCompareRec cR c (TyTupleF xs) (TyTupleF ys)
      z -> z

instance ShowRec (TyFTuple ki) where
  liftShowsPrecRec _ slR _ _ n (TyTupleF xs) =
    showsUnaryWith (const slR) "TyTupleF" n xs

instance Bound (TyFTuple ki) where
  TyTupleF tys >>>= f = TyTupleF (fmap (>>= f) tys)

instance Bitransversable (TyFTuple ki) where
  bitransverse fT fL (TyTupleF tys) = TyTupleF <$> traverse (fT fL) tys

class AsTyTuple ki ty where
  _TyTupleP :: Prism' (ty ki j a) (TyFTuple ki j a)

  _TyTuple :: Prism' (Type ki ty a) [Type ki ty a]
  _TyTuple = _Wrapped . _TyAstType . _TyTupleP . _TyTupleF . mapping _Unwrapped

instance AsTyTuple ki TyFTuple where
  _TyTupleP = id

instance {-# OVERLAPPABLE #-} AsTyTuple ki (TySum xs) => AsTyTuple ki (TySum (x ': xs)) where
  _TyTupleP = _TyNext . _TyTupleP

instance {-# OVERLAPPING #-} AsTyTuple ki (TySum (TyFTuple ': xs)) where
  _TyTupleP = _TyNow . _TyTupleP
