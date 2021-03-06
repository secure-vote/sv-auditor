module SV.Light.AuditApp where

import SV.Prelude

import Control.Monad.Aff (Aff, error, sequential, throwError)
import Control.Monad.Aff.Console (log, warn)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExceptT)
import Control.Parallel (parallel)
import Crypt.NaCl (NACL_RANDOM)
import Data.Array (foldr, (:))
import Data.Array as A
import Data.Decimal as Dec
import Data.Either (Either(..), either, fromRight, isRight)
import Data.Foldable (foldl)
import Data.Int (decimal, fromStringAs)
import Data.Lens ((.~), (^.))
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String as String
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.BigNumber (parseBigNumber)
import Network.Ethereum.Core.HexString (takeHex, toBigNumber, toByteString, toHexString)
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Web3 (Address, ChainCursor(..), DLProxy(..), _to, defaultTransactionOptions, embed, fromByteString, mkAddress, mkHexString, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3 as BN
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditBallot (getBallotInfo, getBallotSpec, runBallotCount)
import SV.Light.Delegation (dlgtAddr)
import SV.Light.SCs.ENS (lookupEns)
import SV.Light.SmartContracts (mkTos, runWeb3OrThrow)
import SV.Light.Types.ENS (EnsDetails)
import SV.Light.Types.Eth (Bytes4, uint256Px)
import SV.Types.Lenses (_erc20Addr, getBSpecVer)
import SV.Types.OutboundLogs (StatusUpdate, mkSUFail, mkSULog, mkSUSuccess, mkSUWarn)
import SV.Utils.BigNumber (bnToStr)
import SV.Utils.UInt (uintFromInt)
import SecureVote.Contracts.SVIndex (getBBFarm, getBBFarmID, getBackend)
import SecureVote.Contracts.SVIndexBackend (getDErc20)
import SecureVote.Democs.SwarmMVP.BallotContract (AllDetails, BallotResult, findEthBlockEndingInZeroBefore, noArgs)
import SecureVote.Democs.SwarmMVP.Types (swmBallotShowJustVotes)
import SecureVote.Utils.Array (fromList)
import SecureVote.Utils.Decimal (toFixed)
import SecureVote.Utils.String (padLeft, padRight)
import SecureVote.Utils.Web3Bin (hexToBytesN)
import SecureVote.Web3.Web3 (setProvider)


formatBallotResults :: BallotResult -> String
formatBallotResults {winner, possibleWinners, totals} = resultsMsgHeader <> resultsMsgRest
    where
        resultsMsgHeader = case winner of
            (Just w) -> "Winner! \n" <> formatOpt w
            Nothing -> "Draw!!!\n " <> formatManyOpts possibleWinners
        resultsMsgRest = "\nTotals:\n" <> formatManyOpts totals
        formatManyOpts os = String.joinWith "\n" (map formatOpt os)
        formatOpt (Tuple opt nVotes) = padRight ' ' 30 opt <> " with # votes: " <> padLeft ' ' 30 decStr
            where
                decStr = toFixed 0 nVotes


formatAllDeets :: AllDetails -> String
formatAllDeets {encBallotsWithoutDupes, decryptedBallots, delegateMapNoLoops, ballotMap, balanceMap} =
    formattedResponse
        where
            formattedResponse = String.joinWith "\n" (makeCsvRow rowTitles : csvRows)
            csvRows = map makeCsvRow votersWDetails
            makeCsvRow d = String.joinWith "," [d.voter, d.delegate, d.vote, d.balance]
            votersWDetails = map getVoterDetails voters
            rowTitles = {voter: "Voter", delegate: "Delegate", vote: "Vote", balance: "Balance"}
            getVoterDetails voter =
                { voter: show voter
                , delegate: fromMaybe "No Delegate" $ show <$> (join $ lookup voter delegateMapNoLoops)
                , vote: show $ fromMaybe "Error: no ballot to get" $ either (\err -> "Error: " <> err) swmBallotShowJustVotes <$> lookup voter ballotMap
                , balance: show $ fromMaybe "Error: no balance found" $ toFixed 2 <$> lookup voter balanceMap
                }
            voters = fromList $ Map.keys balanceMap



type AppArgs = { ethUrls :: StrMap String
               , indexEns :: String
               , startingNetwork :: String
               , ensDetails :: EnsDetails
               , ballotId :: String
               , democHash :: String
               , dev :: Boolean}


getEnsAddr :: String -> Maybe Address
getEnsAddr net = case net of
    "1" -> mkAddress =<< mkHexString "0x314159265dD8dbb310642f98f50C066173C1259b"
    "42" -> mkAddress =<< mkHexString "0xd6F4f22eeC158c434b17d01f62f5dF33b108Ae93"
    _ -> Nothing


-- | Main app function for Auditor. Accepts record of parameters needed to audit ballot.
app :: forall eff.
       AppArgs ->
       (StatusUpdate -> Unit) ->
       Aff _ _ -- (Either (Tuple Int String) (Tuple Int BallotResult))
app params@{ethUrls, indexEns, startingNetwork, ensDetails} updateF =
    do
        let usingKovan = startingNetwork == "42"
            netId = startingNetwork
        if usingKovan then uLog "-- DEV MODE (Kovan) --" else pure unit
        providerUrl <- mToAff ("No URL provided for network with ID: " <> startingNetwork) $ StrMap.lookup startingNetwork ethUrls
        liftEff $ setProvider providerUrl

        -- TODO: handle ENS properly
        ensAddr <- mToAff ("No ENS Register address provided for network with ID: " <> startingNetwork) $ mkAddress =<< mkHexString =<< StrMap.lookup startingNetwork ensDetails
        indexAddr <- lookupEns indexEns {network: startingNetwork, address: ensAddr}
        log $ "got index addr: " <> show indexAddr
        democHash <- mToAff "Cannot parse democHash" $ hexToBytesN =<< mkHexString params.democHash
        uLog $ "Using democHash: " <> show democHash
        ballotId <- mToAff ("Cannot parse ballotId to uint256: " <> params.ballotId) $ uIntNFromBigNumber uint256Px =<< parseBigNumber decimal params.ballotId
        log $ "got ballotId: " <> show ballotId
        let ballotIdHex = Hex.padLeft $ toHexString $ unUIntN ballotId
        uLog $ "Using ballotId: " <> show ballotIdHex
        log $ "ballotId first 4 bytes as hex: " <> show (takeHex 8 $ ballotIdHex)
        (bbNamespace :: Bytes4) <- mToAff ("Cannot get namespace (bytes4) from ballotId: " <> show ballotId) $ hexToBytesN $ takeHex 8 $ ballotIdHex
        log $ "got bbNamespace: " <> show bbNamespace
        let ixTos = mkTos indexAddr

        let getErc20 = parallel $ do
                beAddr <- runWeb3OrThrow $ getBackend ixTos Latest
                let beTos = mkTos beAddr
                runWeb3OrThrow $ getDErc20 beTos Latest {democHash}
        let getBBFarmAddr = parallel $ do
                bbFarmId <- runWeb3OrThrow $ getBBFarmID ixTos Latest {bbNamespace}
                runWeb3OrThrow $ getBBFarm ixTos Latest {bbFarmId}

        Tuple erc20Addr bbFarmAddr <- sequential $ Tuple <$> getErc20 <*> getBBFarmAddr

        uLog $ "Using ERC20 address: " <> show erc20Addr
        uLog $ "Votes stored under ballotId at address: " <> show bbFarmAddr

        -- TODO: handle bad ballotId
        let bbFarmLoc = {network: startingNetwork, address: bbFarmAddr}
        bInfo <- getBallotInfo {ballotId, bbFarmLoc}
        bSpec <- getBallotSpec bInfo.bHash

        if getBSpecVer bSpec /= 1 then pure unit else do
            uWarn $ "Warning: This proposal uses BallotSpec v1, which is deprecated. Several fields are ignored including: erc20Addr, startTime, endTime. You can safely ignore this message for proposals voted on before 1st June 2018."

        let bbFarmTos = defaultTransactionOptions # _to .~ Just bbFarmAddr
            ercTos = defaultTransactionOptions # _to .~ Just erc20Addr
            dlgtTos = defaultTransactionOptions # _to .~ Just (dlgtAddr startingNetwork)

        ballotAns <- runExceptT $ runBallotCount {bInfo, bSpec, bbFarmTos, ercTos, dlgtTos, silent: false, netId, dev: netId == "42"} updateF

        let exitC = exitCode ballotAns
        let msgStart = exitMsgHeader exitC
        let msgBody = case ballotAns of
                Left err -> err
                Right {ballotResults} -> "\n\nResults:\n"
                        <> foldl (\rem {name, count, nVotes} -> rem <> "\n" <> name <> " (" <> show nVotes <> "): " <> count) "" ballotResults
        log $ "\n" <> msgStart <> "\n" <> msgBody

        let toRetE = case ballotAns of
                Right bRes@{nVotes, ballotResults} ->
                    (\_ -> Right bRes) $ updateF $ mkSUSuccess {nVotes, ballotResults: mkBResStrMap ballotResults}
                Left err -> (\_ -> Left err) $ updateF $ mkSUFail ("ERROR: " <> err)

        case toRetE of
            Right bRes -> pure $ Right $ Tuple exitC bRes
            Left err -> pure $ Left $ Tuple exitC err
    where
        exitCode e = if isRight e then 0 else 1
        exitMsgHeader exitC = if exitC == 0 then ">>> SUCCESS <<<" else ">>> ERROR <<<"
        mkBResStrMap bRes = StrMap.fromFoldable $ (\{name, count, nVotes} -> Tuple name {count, nVotes}) <$> bRes
        uLog msg = (pure <<< updateF $ mkSULog msg) *> log msg
        uWarn msg = (pure <<< updateF $ mkSUWarn msg) *> warn msg
