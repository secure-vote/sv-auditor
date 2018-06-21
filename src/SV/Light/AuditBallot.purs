module SV.Light.AuditBallot where

import SV.Light.Types.Ballot
import SV.Light.Types.BallotBox
import SV.Light.Types.RunBallot
import SV.Prelude
import SV.Types.Lenses

import Control.Alt ((<|>))
import Control.Monad.Aff (Milliseconds(..), ParAff, catchError, delay, error, forkAff, joinFiber, message, parallel, sequential, throwError)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffC
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, lift)
import Control.Monad.Rec.Class (Step(..), tailRec, tailRecM)
import Control.Parallel (parTraverse)
import Crypt.NaCl (BoxSecretKey)
import Data.Array (concat, foldr, intercalate, last, range)
import Data.Array as Arr
import Data.Array as Array
import Data.Decimal as Dec
import Data.Int (decimal, round, toNumber)
import Data.Int as DInt
import Data.Lens ((.~), (^.), _1, _2)
import Data.Map (Map, fromFoldable, showTree)
import Data.Map as Map
import Data.Newtype (unwrap, wrap)
import Data.Record as R
import Data.Record.ShowRecord (showRecord)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (oneOf, sequence)
import Debug.Trace (class DebugWarning, spy)
import Global.Unsafe (unsafeStringify)
import IPFS (IPFSEff)
import Network.Ethereum.Core.BigNumber (pow, unsafeToInt)
import Network.Ethereum.Core.BigNumber as BN
import Network.Ethereum.Core.HexString (dropHex)
import Network.Ethereum.Web3 (type (:&), Address, BigNumber, Block(..), CallError, ChainCursor(..), D2, D5, D6, DLProxy(..), DOne, ETH, HexString, TransactionOptions, UIntN, _to, defaultTransactionOptions, embed, mkAddress, mkHexString, uIntNFromBigNumber, unAddress, unHex, unUIntN)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getBlockByNumber)
import Network.Ethereum.Web3.Solidity (Tuple10(..), Tuple3(..))
import Network.Ethereum.Web3.Solidity.Size (class KnownSize)
import Network.Ethereum.Web3.Types (Web3)
import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.StatusCode (StatusCode(..))
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Partial.Unsafe (unsafePartial)
import SV.Light.Counts (countBinary, countRange, RangeOffset(..))
import SV.Light.Delegation (getDelegates)
import SV.Light.IPFS (getBlock)
import SV.Light.SCs.BBFarmHelpers (specHash)
import SV.Light.Types.Eth (UInt256, SCLocation, uint256Px)
import SV.Types.OutboundLogs (mkSUBal, mkSUDlgts, mkSUFail, mkSULog, mkSUWarn)
import SV.Utils.BigNumber (bnToDec)
import SecureVote.Contracts.BBFarm (getDetails, getVote)
import SecureVote.Contracts.FakeErc20 (balanceOf, decimals)
import SecureVote.Utils.Array (chunk, fromList, onlyJust)
import SecureVote.Utils.IPFS (hexHashToSha256Bs58)
import SecureVote.Utils.Time (currentTimestamp)
import SecureVote.Utils.Web3Bin (bytesNToHex)
import SecureVote.Web3.Web3 (runWeb3_, zeroAddr, zeroHash)
import Simple.JSON (readJSON')


getBallotInfo :: forall e. {bbFarmLoc :: SCLocation, ballotId :: UInt256} -> Aff (eth :: ETH | Web3Effs e) BallotInfo
getBallotInfo {bbFarmLoc, ballotId} = do
    rawToBallotInfo <$> (w3 $ getDetails tos Latest {ballotId, voter: zeroAddr})
  where
    w3 :: forall e2 a. Web3 _ (Either CallError a) -> Aff _ a
    w3 = (eToAff <=< eToAff <=< runWeb3_)
    -- transaction options
    tos = defaultTransactionOptions # _to .~ Just bbFarmLoc.address
    uintToInt :: forall m a n. KnownSize n => UIntN n -> Int
    uintToInt = unsafeToInt <<< unUIntN
    skCheck a = if a == zeroHash then Nothing else Just a

    rawToBallotInfo (Tuple10 hasVoted nVotesCast secKey subBits startTime endTime specHash deprecated ballotOwner extraData) =
        mkBallotInfo
            ballotId
            (bytesNToHex specHash)
            (uintToInt startTime)
            (uintToInt endTime)
            (skCheck <<< bytesNToHex $ secKey)
            (uintToInt nVotesCast)


getBallotSpec :: forall e. HexString -> Aff (ref :: REF, ipfs :: IPFSEff, buffer :: BUFFER, ajax :: AJAX | e) BallotSpec
getBallotSpec h = do
    -- todo: verify hash
    (exceptToAff <<< readJSON') =<< _getBallotSpec
  where
    getIpfs = do
        block <- getBlock (hexHashToSha256Bs58 h)
        (liftEff $ Buffer.toString UTF8 block."data")
    getHttp = do
        -- 3s delay on getting HTTP for ALT instance on parallel
        delay (Milliseconds 3000.0)
        ajaxGet ("https://archive.secure.vote/" <> filename)
            <|> ajaxGet ("https://archive.test.secure.vote/" <> filename)
      where
        filename = "0x" <> (unHex h) <> ".json"
    _getBallotSpec = sequential $ oneOf
            [ parallel getIpfs
            , parallel getHttp
            ]
    ajaxGet url = do
        resp <- get url
        when (resp.status /= StatusCode 200) do
            throwError (error $ "URL (" <> url <> ") responded with status code (" <> show resp.status <> ")")
        pure resp.response



runBallotCount :: RunBallotArgs -> (_ -> Unit) -> ExceptT String (Aff _) BallotResult
runBallotCount {bInfo, bSpec, bbFarmTos, ercTos, dlgtTos, silent, netId, dev} updateF = do
    nowTime <- lift $ liftEff $ round <$> currentTimestamp
    -- todo: use ballotInfo start and end times (from SC)
    let endTime = bSpec ^. _endTime
        startTime = bInfo.startTime
        tknAddr = unsafePartial fromJust $ ercTos ^. _to
    log $ "Ballot StartTime: " <> show startTime <> ", Ballot EndTime: " <> show endTime <> ", Current Time: " <> show nowTime

    let ballotOptions = bSpec ^. _options

    blocksFibre <- lift $ forkAff $ do
        logAff $ "Finding Eth block close to time: " <> show startTime <> " (takes 10-20 seconds)"
        blocks <- sequential $ Tuple <$> parallel (findEthBlockBefore startTime)
                                         <*> parallel (findEthBlockBefore $ min nowTime endTime)
        logAff $ "Using start/end blocks " <> show blocks <> " for ERC20 balances and delegation."
        pure blocks

    -- check that we can proceed
    let encPkM = bSpec ^. _encryptionPK
        secKey = bInfo.encSecKey
    case (Tuple (nowTime < endTime) (isNothing encPkM)) of
        Tuple true true -> warn "Ballot has not ended, determining live results and using current delegations..."
        Tuple true false -> throwError "Error: The ballot has ended but I cannot determine the results due as the secret key has not been released"
        _ -> pure unit

    case secKey of
        Nothing -> log "Ballot is not encrypted, proceeding..."
        Just sk -> log $ "The ballot encryption secret key is " <> show sk

    let nVotes = bInfo.nVotesCast
    log $ "Ballot smart contract reports " <> show nVotes <> " were cast."

    log "Retrieving votes now. This may take some time."
    ballotProgress <- lift $ makeVar 0
    rawBallotsWDupes <- lift $ getBallots bbSC bInfo (incrementBallotProgress nVotes logAff ballotProgress)
    log $ "Retrieved " <> (show $ Arr.length rawBallotsWDupes) <> " votes"

    rawBallotsNoDupes <- removeDupes rawBallotsWDupes
    log $ "Removing repeated votes took nVotes from " <>
            (show $ Arr.length rawBallotsWDupes) <> " to " <>
            (show $ Arr.length rawBallotsNoDupes)

    plaintextBallots <- if isJust (bSpec ^. _encryptionPK) then do
            -- decryptedBallots <- lift $ decryptBallots secKey rawBallotsNoDupes logAff
            -- log $ "Decrypted " <> (show $ Arr.length decryptedBallots) <> " votes successfully"
            -- pure decryptedBallots
            throwError "Ballot decryption not yet supported"
        else pure rawBallotsNoDupes

    let ballotMap = Map.fromFoldable $ (\b@{voterAddr} -> Tuple voterAddr b) <$> plaintextBallots

    Tuple ballotStartBlock ballotEndBlock <- lift $ joinFiber blocksFibre
    let ballotStartCC = BN $ wrap $ embed ballotStartBlock

    log $ "Finding all delegates..."
    delegateMap <- lift $ getDelegates {tknAddr, allBallots: plaintextBallots} dlgtSC (BN $ wrap $ embed ballotEndBlock)
    log $ "Found " <> show (Map.size delegateMap) <> " relevant delegations"
    lift $ logDelegates delegateMap

    log $ "Getting balances for all addresses..."
    let allVoters = (\{voterAddr} -> voterAddr) <$> plaintextBallots
    -- also get all ppl who have made some delegation that relates to some vote
    let allRelevantTknHolders = fromList $ Map.keys delegateMap
    balanceMap <- lift $ removeBannedAddrs <$> getBalances ercSC ballotStartBlock (allVoters <> allRelevantTknHolders)
    log $ "Got " <> show (Map.size balanceMap) <> " total balances"
    lift $ logBalances balanceMap

    log $ "Calculating weighted ballots according to ERC20 balances..."
    -- | loop through addrs in balance map to find the first associated vote and associate balances
    let (weightedBallots :: Array GetVoteResult) = onlyJust $ getVoteOrRecurse ballotMap delegateMap <$> Map.toUnfoldable balanceMap
    logDev $ "Weighted Ballots: \n" <> intercalate "\n" ( showRecord <$> weightedBallots )

    -- | allow us to adjust totals in weightedBallotsPre according to ERC20 DPS
    dps <- lift $ (unsafeToInt <<< unUIntN) <$> ercSC (web3NoArgs decimals) Latest unit

    log $ "Calculating final results..."
    let ballotResults =
            getResults ballotOptions weightedBallots
            -- modify values to show a decimal string according to ERC20 DPS
            <#> R.modify (SProxy :: SProxy "count")
                (bnToDec >>> flip (/) (Dec.pow (Dec.fromInt 10) $ Dec.fromInt dps) >>> Dec.toFixed dps)

    pure {nVotes, ballotResults}
  where
    w3BB :: forall a b. (TransactionOptions _ -> b -> Web3 _ (Either CallError a)) -> b -> ExceptT String (Aff _) a
    w3BB r = w3Gen r bbFarmTos
    w3Erc r = w3Gen r ercTos
    w3Gen :: forall a b. (TransactionOptions _ -> b -> Web3 _ (Either CallError a)) -> TransactionOptions _ -> b -> ExceptT String (Aff _) a
    w3Gen r tos = convE <<< lift <<< (eToAff <=< runWeb3_) <<< r tos

    bbSC :: forall args e a. SmartContract e args a
    bbSC = genericSC bbFarmTos

    ercSC :: forall args e a. SmartContract e args a
    ercSC = genericSC ercTos

    dlgtSC :: forall args e a. SmartContract e args a
    dlgtSC = genericSC dlgtTos

    genericSC :: forall args e a. TxOpts -> SmartContract e args a
    genericSC tos f c args = eToAff <=< eToAff <=< runWeb3_ $ f tos c args

    log :: forall e. String -> ExceptT String (Aff _) Unit
    log str = lift $ logAff str

    logBalances :: BalanceMap -> Aff _ Unit
    logBalances = pure <<< updateF <<< spyDev <<< mkSUBal

    logDelegates :: DelegateMap -> Aff _ Unit
    logDelegates = pure <<< updateF <<< spyDev <<< mkSUDlgts

    spyDev :: forall a. a -> a
    spyDev a = if netId == "42" then spy a else a

    logDev str = if dev then log str else pure unit

    logAff :: forall e. String -> Aff _ Unit
    logAff str = do
        let _ = updateF $ mkSULog str
        if silent then pure unit else AffC.log str

    warnAff str = do
        let _ = updateF $ mkSUWarn str
        if silent then pure unit else AffC.warn str

    failAff str = do
        let _ = updateF $ mkSUFail str
        if silent then pure unit else AffC.error str


    warn :: String -> ExceptT String (Aff _) Unit
    warn = lift <<< warnAff
    fail :: String -> ExceptT String (Aff _) Unit
    fail = lift <<< failAff

    convE :: forall m a. MonadThrow String m => m (Either CallError a) -> m a
    convE me = either (throwError <<< (<>) "Web3 Call Error: " <<< show) pure =<< me

    web3NoArgs f txOpts cc _ = f txOpts cc

    adjustBal dps = (*) (pow (embed 10) $ 0 - dps)


-- | Log and increment the number of ballots we've processed to facilitate progress updates
incrementBallotProgress :: forall e. Int -> (String -> Aff (avar :: AVAR | e) Unit) -> AVar Int -> Aff (avar :: AVAR | e) Unit
incrementBallotProgress totalBallots log avar = do
    n <- (+) 1 <$> takeVar avar
    putVar n avar
    if n `mod` 10 == 0
        then log $ "Processed " <> show n <> " ballots; " <> show (n * 100 / totalBallots) <> "% done."
        else pure unit


-- | Get ballots from SC
getBallots :: forall e m. (forall args a. SmartContract _ args a) -> BallotInfo -> Aff _ Unit -> Aff _ (Array BallotFromSC)
getBallots bbSC {ballotId, nVotesCast} incBallotProgress
    | nVotesCast <= 0 = pure []
    | otherwise = do
        let allVoteIds = range 0 (nVotesCast-1)
        let (chunks :: Array (Array Int)) = chunk 25 allVoteIds
        let (toPar :: Array (Aff _ (Array BallotFromSC))) = (parTraverse getBallot) <$> chunks
        map concat $ sequence $ toPar
    where
        getBallot i = do
            ballotN <- mToAff ("Unable to convert " <> show i <> " to uint256!") $ uIntNFromBigNumber uint256Px $ embed i
            -- TODO: should we use a chaincursor based on endTime here instead of latest?
            (Tuple3 ballotBytesN voterAddr extraData) <- bbSC getVote Latest {ballotId, voteId: ballotN}
            -- TODO: support voterPks for encrypted ballots
            -- voterPk <- bytesNToHex <$> bbSC curve25519Pubkeys Latest ballotN
            let ballot = bytesNToHex ballotBytesN
                -- TODO: handle SV ID case (non-ethereum sending addresses)
            incBallotProgress
            pure $ {i, ballot, voterPk: zeroHash, voterAddr}


-- todo, this is going to be slow -- might be much faster to reverse the array...
-- implemented as per 4.9.3 in Swarm Voting Spec
removeDupes :: forall e m. MonadThrow String (m (Aff e)) => Array BallotFromSC -> m (Aff e) (Array BallotFromSC)
removeDupes [] = pure []
removeDupes ballots = do
        lastBallot <- maybe (throwError "ballots did not have a `last` element - but we checked for that!") pure $ last ballots
        let remBallots = Arr.filter (\b -> b.voterAddr /= lastBallot.voterAddr) ballots
        (<>) <$> (removeDupes remBallots) <*> pure [lastBallot]


-- -- | Decrypt those ballots
-- decryptBallots :: forall e e2. BoxSecretKey -> Array BallotFromSC -> (String -> Aff (console :: CONSOLE | e) Unit) -> Aff (console :: CONSOLE | e) (Array BallotFromSC)
-- decryptBallots _ [] log = pure []
-- decryptBallots encSk ballots log = do
--         ballots_ <- parTraverse decryptOne ballots
--         let cleanBallots = map (unsafePartial $ fromRight) (filter isRight ballots_)
--         let badDecryptions = map (unsafePartial $ fromLeft) (filter isLeft ballots_)
--         if length badDecryptions > 0 then
--                 log $ "Got " <> (show $ length badDecryptions) <> " bad decryptions: " <> show badDecryptions
--             else
--                 log "All ballots decrypted successfully."
--         pure cleanBallots
--     where
--         decryptOne {i, encBallot, voterPk, voterAddr} = do
--             let ballotM = toUint8Array <$> (decryptOneTimeBallot (toBox encBallot) (toBoxPubkey voterPk) encSk)
--             maybe (pure $ Left $ "Cannot decrypt ballot from: " <> show voterAddr) (\ballot -> pure $ Right {ballot, voterPk, voterAddr}) ballotM


-- | Get balances
getBalances :: forall m e. SmartContract e {_owner :: Address} UInt256 -> Int -> Array Address -> Aff (eth :: ETH | Web3Effs e) BalanceMap
getBalances w3Erc blockNumber ballots = do
        pairs <- parTraverse addBalance ballots
        pure $ fromFoldable pairs
    where
        addBalance voterAddr = do
            balance <- unUIntN <$> w3Erc balanceOf (BN $ wrap $ embed blockNumber) {_owner: voterAddr}
            pure $ Tuple voterAddr balance

bannedAddrs :: Set.Set Address
bannedAddrs = Set.fromFoldable $ (unsafePartial fromJust <<< (mkAddress <=< mkHexString)) <$>
    [ "0x8bf7b2d536d286b9c5ad9d99f608e9e214de63f0" -- SWM Foundation
    ]

removeBannedAddrs :: Map Address _ -> Map Address _
removeBannedAddrs = Map.filterKeys (\k -> not $ Set.member k bannedAddrs)


findEthBlockBefore :: Int -> Aff _ Int
findEthBlockBefore targetTime = do
    let initLowBlock = 0
    currBlock <- runWeb3_ eth_blockNumber >>= eToAff <#> (unwrap >>> unsafeToInt)
    Tuple currBlockTs lowTs <- sequential $ Tuple
        <$> parallel (getBlockTimestamp currBlock)
        <*> parallel (getBlockTimestamp initLowBlock)

    -- take a guess first to try and get close
    let ts1Scaled = (toNumber $ (targetTime - lowTs) / 100000)
        ts2Scaled = (toNumber $ (currBlockTs - lowTs) / 100000)
        bScaled = (toNumber $ (currBlock - initLowBlock) / 1000)
    let midStep = (ts1Scaled * bScaled) / ts2Scaled
    let finalStepF = add initLowBlock <<< mul 1000 <<< DInt.round
    let gBH = min (finalStepF $ midStep * 1.1) currBlock
    let gBL = max (finalStepF $ midStep * 0.9) initLowBlock
    Tuple gHTs gLTs <- sequential $ Tuple <$> parallel (getBlockTimestamp gBH) <*> parallel (getBlockTimestamp gBL)

    AffC.log $ "Searching for block with targetTs: " <> show targetTime <> ", currBlockTs: " <> show currBlockTs <> ", currBlock: " <> show currBlock
    let runF = _findLastEthBlockBefore targetTime
    if currBlockTs < targetTime || targetTime < lowTs
        then throwError $ error $ "Cannot find Eth block at " <> show targetTime <> " because it is outside range: " <> show lowTs <> ", " <> show currBlockTs
        else case Tuple (compare gHTs targetTime) (compare gLTs targetTime) of
            -- if upper guess is LT target time
            Tuple LT _ -> runF {hTs: currBlockTs, hB: currBlock, lTs: gHTs, lB: gBH}
            -- if lower guess is GT target time
            Tuple _ GT -> runF {hTs: gLTs, hB: gBL, lTs: lowTs, lB: initLowBlock}
            -- if we hit the money in any way
            Tuple EQ _ -> pure gBH
            Tuple _ EQ -> pure gBL
            -- otherwise we're in between
            Tuple GT LT -> runF {hTs: gHTs, hB: gBH, lTs: gLTs, lB: gBL}
  where
    _findLastEthBlockBefore :: Int -> {hTs :: Int, hB :: Int, lTs :: Int, lB :: Int} -> Aff _ Int
    _findLastEthBlockBefore tTime {hTs, hB, lTs, lB} = do
        case compare hB lB of
                LT -> go tTime lTs lB hTs hB
                EQ -> pure lB
                GT -> go tTime hTs hB lTs lB
      where
        go tTime hTs hB lTs lB = do
            AffC.log $ "Block search: blockN diff: " <> show (hB - lB) <> ", Target: " <> show tTime

            let testBlockN = (hB - lB) / 2 + lB
            newTs <- getBlockTimestamp testBlockN

            case compare newTs tTime of
                GT -> _findLastEthBlockBefore tTime {hTs: newTs, hB: testBlockN, lTs, lB}
                EQ -> pure $ testBlockN
                LT -> if hB - lB == 1 then pure $ lB else _findLastEthBlockBefore tTime {hTs, hB, lTs: newTs, lB: testBlockN}
    getBlockTimestamp blkNum = runWeb3_ (eth_getBlockByNumber (BN $ wrap $ embed blkNum)) >>= eToAff <#> (\(Block b) -> b.timestamp # unsafeToInt)


type GetVoteLoopInput = {ballotMap :: BallotMap, delegateMap :: DelegateMap, p :: {origVoter :: Address, bal :: BigNumber, vtr :: Address}}

-- | This takes a ballotMap, delegateMap, and a (voter, balance) - it'll find the _first_ ballot in the
-- | delegation chain and associate the balance with that ballot.
getVoteOrRecurse :: BallotMap -> DelegateMap -> Tuple Address BigNumber -> Maybe GetVoteResult
getVoteOrRecurse ballotMap delegateMap p@(Tuple origVoter origBal) = do
    ret <- tailRecM go {ballotMap, delegateMap, p: {origVoter, vtr: origVoter, bal: origBal}}
    let _ = unsafePerformEff $ EffC.log $ "Returning for voter " <> show ret.origVoter <> " balance " <> show (ret.bal) <> " with ballot " <> unsafeStringify (ret.ballot)
    pure ret
  where
    go :: GetVoteLoopInput -> Maybe (Step _ GetVoteResult)
    go {ballotMap, delegateMap, p: p@{origVoter, bal, vtr}} = case Map.lookup vtr ballotMap of
            Just ballot -> pure $ Done $ {origVoter, ballot, bal}
            Nothing -> do
                dlgt <- Map.lookup vtr delegateMap
                pure $ Loop {ballotMap, delegateMap, p: p {vtr = dlgt}}


-- | Take the BallotOptions and weighted ballots and give back results
getResults :: OptsOuter -> Array GetVoteResult -> Array BallotOptResult
getResults ballotOpts weightedBallots = case ballotOpts of
        OptsBinary -> countBinary weightedBallots
        OptsSimple simpleType opts -> case simpleType of
            RangeVotingPlusMinus3 -> countRange (RangePlusMinus {magnitude: 3}) opts weightedBallots
