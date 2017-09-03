module Test.Data.Scannable where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Gen.Class (class MonadGen, chooseBool, chooseInt)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Maybe.Last (Last(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Scannable (verifyScannable)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

genInt ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m Int
genInt = chooseInt bottom top

genDisj ∷ ∀ m. MonadRec m ⇒ MonadGen m ⇒ m (Disj Boolean)
genDisj = Disj <$> chooseBool

verifyAdditive ∷ Gen Boolean
verifyAdditive =
  verifyScannable (+) (_ + 1) <$> genInt <*> (Additive <$> genInt)

verifyMultiplicative ∷ Gen Boolean
verifyMultiplicative =
  verifyScannable (*) (_ * 10) <$> genInt <*> (Multiplicative <$> genInt)

verifyConj ∷ Gen Boolean
verifyConj =
  verifyScannable (&&) (_ && true) <$> chooseBool <*> (Conj <$> chooseBool)

verifyDisj ∷ Gen Boolean
verifyDisj =
  verifyScannable (&&) (_ && true) <$> chooseBool <*> (Disj <$> chooseBool)

verifyDual ∷ Gen Boolean
verifyDual =
  verifyScannable (<>) (_ <> (Disj true)) <$> (Disj <$> chooseBool)
                                          <*> (Dual <<< Disj <$> chooseBool)

verifyFirst ∷ Gen Boolean
verifyFirst =
  verifyScannable (+) ((+) 0) <$> genInt <*> (First <<< Just <$> genInt)

verifyLast ∷ Gen Boolean
verifyLast =
  verifyScannable (+) ((+) 0) <$> genInt <*> (Last <<< Just <$> genInt)


main ∷ Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  let exclaim a = a <> "!"
  describe "Scannable (Maybe String)" do
    it "should satisfy the Scannable laws" do
      quickCheck \(x ∷ Maybe String) (y ∷ String) →
        verifyScannable (\_ → exclaim) exclaim y x

  describe "Scannable Array" do
    it "shoold satisfy the Scannable laws" do
      quickCheck \(x ∷ Array String) (y ∷ String) →
        verifyScannable (\_ → exclaim) exclaim y x

  describe "Scannable Additive" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyAdditive

  describe "Scannable Multiplicative" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyMultiplicative

  describe "Scannable Disj" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyDisj

  describe "Scannable Conj" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyConj

  describe "Scannable Dual" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyDual

  describe "Scannable First" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyFirst

  describe "Scannable Last" do
    it "shoold satisfy the Scannable laws" do
      quickCheck verifyLast
