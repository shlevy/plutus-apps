{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Plutus.Contract.Test.Certification where

import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import PlutusTx.Coverage
import Test.Tasty as Tasty

data Instance c m where
  Instance :: c m => Instance c m

data Certification m = Certification {
    certCoverageIndex      :: CoverageIndex,
    certNoLockedFunds      :: Maybe (NoLockedFundsProof m),
    certNoLockedFundsLight :: Maybe (NoLockedFundsProofLight m),
    certUnitTests          :: Maybe TestTree,
    certCrashTolerance     :: Maybe (Instance CrashTolerance m),
    certWhitelist          :: Maybe Whitelist
  }

defaultCertification :: Certification m
defaultCertification = Certification { certCoverageIndex = mempty
                                     , certNoLockedFunds = Nothing
                                     , certNoLockedFundsLight = Nothing
                                     , certUnitTests = Nothing
                                     , certCrashTolerance = Nothing
                                     , certWhitelist = Just defaultWhitelist }
