{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Plutus.Contract.Test.Certification.Run
  ( -- * A certification report holds all the necessary information
    -- to make sense of certification results
    CertificationReport
  -- * There are a tonne of lenses
  , certRes_standardPropertyResult
  , certRes_noLockedFundsResult
  , certRes_noLockedFundsLightResult
  , certRes_standardCrashToleranceResult
  , certRes_unitTestResults
  , certRes_coverageReport
  , certRes_coverageIndexReport
  , certRes_whitelistOk
  , certRes_whitelistResult
  -- * and we have a function for running certification
  , certify
  ) where

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
    _certRes_standardPropertyResult       :: QC.Result,
    _certRes_noLockedFundsResult          :: Maybe QC.Result,
    _certRes_noLockedFundsLightResult     :: Maybe QC.Result,
    _certRes_standardCrashToleranceResult :: Maybe QC.Result,
    _certRes_unitTestResults              :: [Tasty.Result],
    _certRes_coverageReport               :: CoverageReport,
    _certRes_coverageIndexReport          :: CoverageIndex,
    _certRes_whitelistOk                  :: Maybe Bool,
    _certRes_whitelistResult              :: Maybe QC.Result
  } deriving Show

makeLenses ''CertificationReport

runStandardProperty :: forall m. ContractModel m => Int -> CoverageIndex -> IO (CoverageReport, QC.Result)
runStandardProperty n covIdx =
  quickCheckWithCoverageAndResult (set coverageIndex covIdx defaultCoverageOptions) $ \ covopts ->
    withMaxSuccess n $
    propRunActionsWithOptions @m defaultCheckOptionsContractModel
                                 covopts
                                 (const (pure True))

checkNoLockedFunds :: ContractModel m => Int -> NoLockedFundsProof m -> IO QC.Result
checkNoLockedFunds n prf = quickCheckResult
                         $ withMaxSuccess n
                         $ checkNoLockedFundsProof defaultCheckOptionsContractModel prf

checkNoLockedFundsLight :: ContractModel m => Int -> NoLockedFundsProofLight m -> IO QC.Result
checkNoLockedFundsLight n prf = quickCheckResult
                              $ withMaxSuccess n
                              $ checkNoLockedFundsProofLight prf

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

checkDerived :: forall d m c. (c m => ContractModel (d m))
             => Maybe (Instance c m)
             -> Int
             -> CoverageIndex
             -> IO (CoverageReport, Maybe QC.Result)
checkDerived Nothing _ _                     = return (mempty, Nothing)
checkDerived (Just Instance) numTests covIdx = second Just <$> runStandardProperty @(d m) numTests covIdx

checkWhitelist :: forall m. ContractModel m
               => Maybe Whitelist
               -> Int
               -> CoverageIndex
               -> IO (CoverageReport, Maybe QC.Result)
checkWhitelist Nothing _ _ = return (mempty, Nothing)
checkWhitelist (Just wl) numTest covIdx = do
  (cov, res) <- quickCheckWithCoverageAndResult (set coverageIndex covIdx defaultCoverageOptions) $
                  \ covopts -> withMaxSuccess numTest $ checkErrorWhitelistWithOptions @m
                                                            defaultCheckOptionsContractModel
                                                            covopts wl
  return (cov, Just res)

certify :: forall m. ContractModel m => Certification m -> IO (CertificationReport m)
certify Certification{..} = do
  let numTests = 100
  -- Unit tests
  unitTests    <- fromMaybe [] <$> traverse runUnitTests certUnitTests
  -- Standard property
  (cov, qcRes) <- runStandardProperty @m numTests certCoverageIndex
  -- No locked funds
  noLock       <- traverse (checkNoLockedFunds numTests) certNoLockedFunds
  -- No locked funds light
  noLockLight  <- traverse (checkNoLockedFundsLight numTests) certNoLockedFundsLight
  -- Crash tolerance
  (cov', ctRes) <- checkDerived @WithCrashTolerance certCrashTolerance numTests certCoverageIndex
  -- Whitelist
  (cov'', wlRes) <- checkWhitelist @m certWhitelist numTests certCoverageIndex
  -- Final results
  return $ CertificationReport { _certRes_standardPropertyResult       = qcRes,
                                 _certRes_standardCrashToleranceResult = ctRes,
                                 _certRes_noLockedFundsResult          = noLock,
                                 _certRes_noLockedFundsLightResult     = noLockLight,
                                 _certRes_unitTestResults              = unitTests,
                                 _certRes_coverageReport               = cov <> cov' <> cov'',
                                 _certRes_coverageIndexReport          = certCoverageIndex,
                                 _certRes_whitelistOk                  = whitelistOk <$> certWhitelist,
                                 _certRes_whitelistResult              = wlRes }
