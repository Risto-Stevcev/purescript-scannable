module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Data.Scannable (main) as Test
import Test.Spec.QuickCheck (QCRunnerEffects)

main ∷ Eff (QCRunnerEffects ()) Unit
main = Test.main
