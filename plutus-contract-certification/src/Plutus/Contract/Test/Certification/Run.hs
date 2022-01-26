{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

module Plutus.Contract.Test.Certification.Run where

import Control.Arrow
import Control.Concurrent.STM
import Control.Lens
import Data.IntMap qualified as IntMap
import Data.Maybe
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import PlutusTx.Coverage
import Test.QuickCheck as QC
import Test.Tasty as Tasty
import Test.Tasty.Runners as Tasty

data CertificationReport m = CertificationReport {
    standardPropertyResult       :: QC.Result,
    noLockedFundsResult          :: Maybe QC.Result,
    standardCrashToleranceResult :: Maybe QC.Result,
    unitTestResults              :: [Tasty.Result],
    coverageReport               :: CoverageReport,
    coverageIndexReport          :: CoverageIndex
  } deriving Show

runStandardProperty :: forall m. ContractModel m => Int -> CoverageIndex -> IO (CoverageReport, QC.Result)
runStandardProperty n covIdx =
  quickCheckWithCoverageAndResult (set coverageIndex covIdx defaultCoverageOptions) $ \ covopts ->
    withMaxSuccess n $
    propRunActionsWithOptions @m defaultCheckOptionsContractModel
                                 covopts
                                 (const (pure True))

checkNoLockedFunds :: ContractModel m => Int -> NoLockedFundsProof m -> IO QC.Result
checkNoLockedFunds n prf = quickCheckResult $ withMaxSuccess n $ checkNoLockedFundsProof defaultCheckOptionsContractModel prf

runUnitTests :: TestTree -> IO [Tasty.Result]
runUnitTests t = launchTestTree mempty t $ \ status -> do
    rs <- atomically $ mapM waitForDone (IntMap.elems status)
    return $ \ _ -> return rs
  where
    waitForDone tv = do
      s <- readTVar tv
      case s of
        Done r -> return r
        _      -> retry

certify :: forall m. ContractModel m => Certification m -> IO (CertificationReport m)
certify Certification{..} = do
  unitTests    <- fromMaybe [] <$> traverse runUnitTests certUnitTests
  (cov, qcRes) <- runStandardProperty @m 100 certCoverageIndex
  noLock       <- traverse (checkNoLockedFunds 100) certNoLockedFunds
  (cov', ctRes) <- case certCrashTolerance of
    Just Instance -> second Just <$> runStandardProperty @(WithCrashTolerance m) 100 certCoverageIndex
    Nothing       -> return (mempty, Nothing)
  return $ CertificationReport { standardPropertyResult       = qcRes,
                                 standardCrashToleranceResult = ctRes,
                                 noLockedFundsResult          = noLock,
                                 unitTestResults              = unitTests,
                                 coverageReport               = cov <> cov',
                                 coverageIndexReport          = certCoverageIndex }
