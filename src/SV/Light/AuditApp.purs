module SV.Light.AuditApp where

import SV.Prelude

import Control.Monad.Aff (Aff, throwError, error)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Except (runExceptT)
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
import Network.Ethereum.Core.HexString (toBigNumber)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Web3 (Address, ChainCursor(..), DLProxy(..), _to, defaultTransactionOptions, embed, mkAddress, mkHexString, uIntNFromBigNumber)
import Network.Ethereum.Web3 as BN
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditBallot (getBallotInfo, getBallotSpec, runBallotCount)
import SV.Light.Delegation (dlgtAddr)
import SV.Light.SCs.ENS (lookupEns)
import SV.Light.SmartContracts (mkTos, runWeb3OrThrow)
import SV.Light.Types.ENS (EnsDetails)
import SV.Types.Lenses (_erc20Addr)
import SV.Types.OutboundLogs (StatusUpdate, mkSUFail, mkSULog, mkSUSuccess)
import SV.Utils.BigNumber (bnToStr)
import SV.Utils.UInt (uintFromInt)
import SecureVote.Contracts.SVIndex (getBBFarm)
import SecureVote.Democs.SwarmMVP.BallotContract (AllDetails, BallotResult, findEthBlockEndingInZeroBefore, noArgs)
import SecureVote.Democs.SwarmMVP.Types (swmBallotShowJustVotes)
import SecureVote.Utils.Array (fromList)
import SecureVote.Utils.Decimal (toFixed)
import SecureVote.Utils.String (padLeft, padRight)
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



type AppArgs = {ethUrls :: StrMap String, indexEns :: String, startingNetwork :: String, ensDetails :: EnsDetails, ballotId :: String, dev :: Boolean}


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
app params@{ethUrls, indexEns, startingNetwork, ensDetails, dev} updateF =
    do
        if startingNetwork == "42" then log "-- DEV MODE (Kovan) --" else pure unit
        let providerUrl = fromMaybe "" $ StrMap.lookup startingNetwork ethUrls
        liftEff $ setProvider providerUrl

        -- TODO: handle ENS properly
        ensAddr :: Address <- mToAff "Unable to get the ENS Register address" $ getEnsAddr startingNetwork
        indexAddr <- lookupEns indexEns {network: startingNetwork, address: ensAddr}
        log $ "got index addr: " <> show indexAddr
        -- TODO: handle bbfarms other than 0
        bbFarmAddr <- runWeb3OrThrow $ getBBFarm (mkTos indexAddr) Latest {bbFarmId: (uintFromInt 0)}

        -- TODO: handle bad ballotId
        let ballotId = unsafePartial fromJust $ uIntNFromBigNumber DLProxy <<< toBigNumber =<< mkHexString params.ballotId
            bbFarmLoc = {network: startingNetwork, address: bbFarmAddr}
        bInfo <- getBallotInfo {ballotId, bbFarmLoc}
        bSpec <- getBallotSpec bInfo.bHash
        let bbFarmTos = defaultTransactionOptions # _to .~ Just bbFarmAddr
            ercTos = defaultTransactionOptions # _to .~ Just (bSpec ^. _erc20Addr)
            dlgtTos = defaultTransactionOptions # _to .~ Just (dlgtAddr {dev})

        ballotAns <- runExceptT $ runBallotCount {bInfo, bSpec, bbFarmTos, ercTos, dlgtTos, silent: false, dev} updateF

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
