module Plutus.Contract.Test.Certification where

import Plutus.Contract.Test.ContractModel
import PlutusTx.Coverage
import Test.Tasty as Tasty

data Certification m = Certification {
    certNoLockedFunds :: Maybe (NoLockedFundsProof m),
    certUnitTests     :: Maybe TestTree,
    certCoverageIndex :: CoverageIndex
  }
