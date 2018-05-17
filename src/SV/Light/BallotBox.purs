module SV.Light.BallotBox where

import SV.Prelude

import Control.Monad.Aff (Aff, ParAff, error, parallel, sequential, throwError)
import Control.Monad.Eff.Ref (REF)
import Data.Lens ((.~), (^.))
import Data.Map as Map
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (class KnownSize, CallError, ChainCursor(..), ETH, UIntN, _to, defaultTransactionOptions, unUIntN)
import Network.Ethereum.Web3.Types.Types (Web3(..))
import SV.Light.SmartContract (scRespOkay)
import SV.Light.Types.RunBallot (BallotBoxVersion(..), BallotInfo, BallotOperations, mkBallotInfo)
import SV.Utils.Solidity (uint256ToIntUnsafe, uintToIntUnsafe)
import SecureVote.Contracts.BallotBoxVersion2 (specHash)
import SecureVote.Contracts.SVLightBallotBox (getVersion)
import SecureVote.Utils.Web3Bin (bytesNToHex)
import SecureVote.Web3.Web3 (runWeb3_, zeroHash)


determineBallotBoxVersion :: Address -> Aff _ BallotBoxVersion
determineBallotBoxVersion addr = do
    -- version criteria:
    -- if getVersion doesn't exist and specHash does exist then v1
    -- if neither exist then v2
    -- otherwise v<getVersion()>
    Tuple bVerEE hasOldSpecHash <- sequential $ Tuple
            <$> parallel (runWeb3_ $ getVersion tos Latest)
            <*> parallel (map scRespOkay $ runWeb3_ $ specHash tos Latest)

    let hasOldSpecHash = false
        bVer = case bVerEE of
            Left w3e -> throwError $ error ""
            Right bVerE -> case bVerE of
                    Left _ -> if hasOldSpecHash then BVer 2 else BVer 1
                    Right v -> BVer $ uint256ToIntUnsafe v

    pure bVer
  where
    tos = defaultTransactionOptions # _to ^. Just addr



getBallotOps :: Address -> Aff _ (BallotOperations _)
getBallotOps addr = do
    -- todo: build out functions here for multi-ballot versions
    -- check if we're on a legacy ballot (swm-v1)
    -- check if we're using swm-v2 (first ballot spec options)
    -- check if we're using v3+
    bVer <- determineBallotBoxVersion addr

    case bVer of
        BVer 1 -> ballotOpsV1 addr
        BVer 2 -> ballotOpsV2 addr
        BVer n -> genBallotOpts addr n


ballotOpsV1 :: Address -> Aff _ (BallotOperations _)
ballotOpsV1 addr =
    pure { getBallots: (\i inc -> pure [])
         , ballotOk: (pure true)
         , getDelegateMap: pure Map.empty
         , getBalanceMap: pure Map.empty
         }


ballotOpsV2 :: Address -> Aff _ (BallotOperations _)
ballotOpsV2 addr =
    pure { getBallots: (\i inc -> pure [])
         , ballotOk: (pure true)
         , getDelegateMap: pure Map.empty
         , getBalanceMap: pure Map.empty
         }


genBallotOpts :: Address -> Int -> Aff _ (BallotOperations _)
genBallotOpts addr n
    | n < 1     = throwError $ error $ "Invalid ballot version: " <> show n <> ". Bailing"
    | n == 1    = ballotOpsV1 addr
    | n == 2    = ballotOpsV2 addr
    | otherwise = do
        pure ballotOpsV2 addr



getBallotInfo :: forall e. {bScAddr :: Address} -> Aff (eth :: ETH, ref :: REF | e) BallotInfo
getBallotInfo {bScAddr} = do
    -- todo: test version stuff for ballot
    bVer <- determineBallotBoxVersion bScAddr

    sequential $
        mkBallotInfo <$> (map bytesNToHex $ pw3 $ _specHash bVer tos Latest)
                     <*> (map uintToIntUnsafe $ pw3 $ _startTime bVer tos Latest)
                     <*> (map uintToIntUnsafe $ pw3 $ _endTime bVer tos Latest)
                     <*> (map (skCheck <<< bytesNToHex) $ pw3 $ _encSeckey bVer tos Latest)
                     <*> (map uintToIntUnsafe $ pw3 $ _nVotesCast bVer tos Latest)
                     <*> (map uintToIntUnsafe $ pw3 $ _creationBlock bVer tos Latest)
                     <*> (parallel $ getBallotOps bScAddr bVer)
  where
    pw3 :: forall e2 a. Web3 (ref :: REF | e2) (Either CallError a) -> ParAff (eth :: ETH, ref :: REF | e2) a
    pw3 = parallel <<< (eToAff <=< eToAff <=< runWeb3_)
    -- transaction options
    tos = defaultTransactionOptions # _to .~ Just bScAddr
    skCheck a = if a == zeroHash then Nothing else Just a
