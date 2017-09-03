module Data.Scannable
  ( class Scannable
  , scanl, scanr
  , scannablePreservesLength, scannableMapping, verifyScannable
  , defaultScanl, defaultScanr
  ) where

import Prelude
import Data.Foldable (class Foldable)
import Data.Foldable (length) as F
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Conj (Conj)
import Data.Monoid.Disj (Disj)
import Data.Monoid.Dual (Dual)
import Data.Monoid.Multiplicative (Multiplicative)
import Data.Traversable (scanl, scanr) as T

-- | The `Scannable` class provides `scanl` and `scanr` functions for structures
-- | that are not `Traversable`.
-- |
-- | Instances should also satisfy the following laws
-- |
-- | If it is also a `Foldable` then it should preserve length:
-- | - `length (scanl f init fa) == length fa`
-- | - `length (scanr (flip f) init fa) == length fa`
-- |
-- | If it is also a `Functor` then it behaves like `map` if the accumulated
-- | argument is ignored:
-- | - `scanl (\_ → f)     _ fa == map f fa`
-- | - `scanr (\a _ → f a) _ fa == map f fa`
class Scannable f where
  scanl ∷ ∀ a b. (b → a → b) → b → f a → f b
  scanr ∷ ∀ a b. (a → b → b) → b → f a → f b

length ∷  ∀ f t. Foldable f ⇒ f t → Int
length = F.length

scannablePreservesLength
  ∷ ∀ f a b. Foldable f ⇒ Scannable f ⇒ (b → a → b) → b → f a → Boolean
scannablePreservesLength f init fa =
  length (scanl f        init fa) == length fa &&
  length (scanr (flip f) init fa) == length fa

scannableMapping
  ∷ ∀ f a b. Eq (f b) ⇒ Functor f ⇒ Scannable f ⇒ (a → b) → b → f a → Boolean
scannableMapping f b fa =
  scanl (\_ → f)     b fa == map f fa &&
  scanr (\a _ → f a) b fa == map f fa

verifyScannable
  ∷ ∀ f a b. Eq (f b) ⇒ Functor f ⇒ Foldable f ⇒ Scannable f
  ⇒ (b → a → b) → (a → b) → b → f a → Boolean
verifyScannable f g i fa =
  scannablePreservesLength f i fa && scannableMapping g i fa

defaultScanl
  ∷ ∀ a b f. ((a → b → b) → b → f a → f b) → (b → a → b) → b → f a → f b
defaultScanl scanr f = scanr (flip f)

defaultScanr
  ∷ ∀ a b f. ((b → a → b) → b → f a → f b) → (a → b → b) → b → f a → f b
defaultScanr scanl f = scanl (flip f)


instance scannableMaybe ∷ Scannable Maybe where
  scanl = T.scanl
  scanr = T.scanr

instance scannableArray ∷ Scannable Array where
  scanl = T.scanl
  scanr = T.scanr

instance scannableAdditive ∷ Scannable Additive where
  scanl = T.scanl
  scanr = T.scanr

instance scannableMultiplicative ∷ Scannable Multiplicative where
  scanl = T.scanl
  scanr = T.scanr

instance scannableDisj ∷ Scannable Disj where
  scanl = T.scanl
  scanr = T.scanr

instance scannableConj ∷ Scannable Conj where
  scanl = T.scanl
  scanr = T.scanr

instance scannableDual ∷ Scannable Dual where
  scanl = T.scanl
  scanr = T.scanr

instance scannableLast ∷ Scannable Last where
  scanl = T.scanl
  scanr = T.scanr

instance scannableFirst ∷ Scannable First where
  scanl = T.scanl
  scanr = T.scanr
