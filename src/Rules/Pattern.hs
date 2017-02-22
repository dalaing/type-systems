{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
module Rules.Pattern (
  ) where

import Control.Lens

import Util

newtype PatternList pt a = PatternList [pt (PatternList pt) a]

makeWrapped ''PatternList

instance Bitransversable pt => Functor (PatternList pt) where
  fmap = fmapDefault

instance Bitransversable pt => Foldable (PatternList pt) where
  foldMap = foldMapDefault

instance Bitransversable pt => Traversable (PatternList pt) where
  traverse f (PatternList pts) = PatternList <$> traverse (bitransverse traverse f) pts

patternToPatternList :: Bitransversable pt => Pattern pt a -> PatternList pt a
patternToPatternList (PtVar _) =
  PatternList [] -- possibly build a wild in here
patternToPatternList (PtTree pt) =
  PatternList
    [ runIdentity .
      bitransverse (\f -> fmap patternToPatternList . traverse f) pure $
      pt
    ]

patternListToPatterns :: Bitransversable pt => PatternList pt a -> [Pattern pt a]
patternListToPatterns (PatternList ps) =
  ps >>= (fmap PtTree . bitransverse (\f -> (>>= patternListToPatterns) . traverse f) pure)

-- pattern list -> [pattern]
-- [pattern] -> pattern list
-- - by grouping by constructor, recursively calling per group, and packing into the PatternList
-- - or by converting each pattern to a singleton pattern list, then merging the lot

data PMatchRule ty pt tm a =
    PMatchBase (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
  | PMatchEval ((Term ty pt tm a -> Term ty pt tm a) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])
  | PMatchRecurse ((Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a])

fixPMatchRule :: (Term ty pt tm a -> Term ty pt tm a) -> (Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]) -> PMatchRule ty pt tm a -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
fixPMatchRule _ _ (PMatchBase f) = f
fixPMatchRule evalFn _ (PMatchEval f) = f evalFn
fixPMatchRule _ pMatchFn (PMatchRecurse f) = f pMatchFn

mkPMatch :: (Term ty pt tm a -> Term ty pt tm a) -> [PMatchRule ty pt tm a] -> Pattern pt a -> Term ty pt tm a -> Maybe [Term ty pt tm a]
mkPMatch innerEval rules x y =
  let
    go p tm =
      asum .
      fmap (\r -> fixPMatchRule innerEval go r p tm) $
      rules
  in
    go x y

data PCheckRule e m pt ty a =
    PCheckBase (Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))
  | PCheckRecurse ((Pattern pt a -> Type ty a -> m [Type ty a]) -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a]))

fixPCheckRule :: (Pattern pt a -> Type ty a -> m [Type ty a]) -> PCheckRule e m pt ty a -> Pattern pt a -> Type ty a -> Maybe (m [Type ty a])
fixPCheckRule _ (PCheckBase f) = f
fixPCheckRule pCheckFn (PCheckRecurse f) = f pCheckFn

mkPCheck :: (MonadError e m, AsUnknownTypeError e) => [PCheckRule e m pt ty a] -> Pattern pt a -> Type ty a -> m [Type ty a]
mkPCheck rules x y =
  let
    go p ty =
      fromMaybe (throwing _UnknownTypeError ()) .
      asum .
      fmap (\r -> fixPCheckRule go r p ty) $
      rules
  in
    go x y

data PAddRule (pt :: (* -> *) -> * -> *) a = PAddRule

mkAdd :: [PAddRule pt a] -> ()
mkAdd _ = ()

data PUncoveredRule (pt :: (* -> *) -> * -> *) a = PUncoveredRule

mkUncovered :: [PUncoveredRule pt a] -> ()
mkUncovered _ = ()

data PatternInput e m ty pt tm a =
  PatternInput {
  }

instance Monoid (PatternInput e m ty pt tm a) where
  mempty = PatternInput
  mappend (PatternInput) (PatternInput) =
    PatternInput

data PatternOutput e m ty pt tm a =
  PatternOutput {
  }
