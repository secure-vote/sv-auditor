--------------------------------------------------------------------------------
-- | BBFarm
--------------------------------------------------------------------------------

module SecureVote.Contracts.BBFarm where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple10, Tuple2(..), Tuple3(..), Tuple5(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | GetCreationTsFn
--------------------------------------------------------------------------------


type GetCreationTsFn = Tagged (SProxy "getCreationTs(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getCreationTs :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getCreationTs x0 cm r = uncurryFields  r $ getCreationTs' x0 cm
   where
    getCreationTs' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getCreationTs' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetCreationTsFn)

--------------------------------------------------------------------------------
-- | SubmitVoteFn
--------------------------------------------------------------------------------


type SubmitVoteFn = Tagged (SProxy "submitVote(uint256,bytes32,bytes)") (Tuple3 (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)) ByteString)

submitVote :: forall e. TransactionOptions NoPay -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), vote :: (BytesN (D3 :& DOne D2)), extra :: ByteString } -> Web3 e HexString
submitVote x0 r = uncurryFields  r $ submitVote' x0
   where
    submitVote' :: TransactionOptions NoPay -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "vote") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extra") ByteString -> Web3 e HexString
    submitVote' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SubmitVoteFn)

--------------------------------------------------------------------------------
-- | SetBallotOwnerFn
--------------------------------------------------------------------------------


type SetBallotOwnerFn = Tagged (SProxy "setBallotOwner(uint256,address)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address)

setBallotOwner :: forall e. TransactionOptions NoPay -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), newOwner :: Address } -> Web3 e HexString
setBallotOwner x0 r = uncurryFields  r $ setBallotOwner' x0
   where
    setBallotOwner' :: TransactionOptions NoPay -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setBallotOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetBallotOwnerFn)

--------------------------------------------------------------------------------
-- | SetDeprecatedFn
--------------------------------------------------------------------------------


type SetDeprecatedFn = Tagged (SProxy "setDeprecated(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setDeprecated :: forall e. TransactionOptions NoPay -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setDeprecated x0 r = uncurryFields  r $ setDeprecated' x0
   where
    setDeprecated' :: TransactionOptions NoPay -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setDeprecated' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetDeprecatedFn)

--------------------------------------------------------------------------------
-- | GetVersionFn
--------------------------------------------------------------------------------


type GetVersionFn = Tagged (SProxy "getVersion()") (Tuple0 )

getVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetVersionFn)

--------------------------------------------------------------------------------
-- | DoLockdownFn
--------------------------------------------------------------------------------


type DoLockdownFn = Tagged (SProxy "doLockdown()") (Tuple0 )

doLockdown :: forall e. TransactionOptions NoPay -> Web3 e HexString
doLockdown x0 = sendTx x0 ((tagged $ Tuple0 ) :: DoLockdownFn)

--------------------------------------------------------------------------------
-- | SetOwnerFn
--------------------------------------------------------------------------------


type SetOwnerFn = Tagged (SProxy "setOwner(address)") (Tuple1 Address)

setOwner :: forall e. TransactionOptions NoPay -> { newOwner :: Address } -> Web3 e HexString
setOwner x0 r = uncurryFields  r $ setOwner' x0
   where
    setOwner' :: TransactionOptions NoPay -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetOwnerFn)

--------------------------------------------------------------------------------
-- | GetNBallotsFn
--------------------------------------------------------------------------------


type GetNBallotsFn = Tagged (SProxy "getNBallots()") (Tuple0 )

getNBallots :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getNBallots x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetNBallotsFn)

--------------------------------------------------------------------------------
-- | HasPermissionsFn
--------------------------------------------------------------------------------


type HasPermissionsFn = Tagged (SProxy "hasPermissions(address)") (Tuple1 Address)

hasPermissions :: forall e. TransactionOptions NoPay -> ChainCursor -> { a :: Address } -> Web3 e (Either CallError Boolean)
hasPermissions x0 cm r = uncurryFields  r $ hasPermissions' x0 cm
   where
    hasPermissions' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "a") Address -> Web3 e (Either CallError Boolean)
    hasPermissions' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: HasPermissionsFn)

--------------------------------------------------------------------------------
-- | GetAdminLogFn
--------------------------------------------------------------------------------


type GetAdminLogFn = Tagged (SProxy "getAdminLog(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getAdminLog :: forall e. TransactionOptions NoPay -> ChainCursor -> { n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getAdminLog x0 cm r = uncurryFields  r $ getAdminLog' x0 cm
   where
    getAdminLog' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getAdminLog' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetAdminLogFn)

--------------------------------------------------------------------------------
-- | IsAdminFn
--------------------------------------------------------------------------------


type IsAdminFn = Tagged (SProxy "isAdmin(address)") (Tuple1 Address)

isAdmin :: forall e. TransactionOptions NoPay -> ChainCursor -> { a :: Address } -> Web3 e (Either CallError Boolean)
isAdmin x0 cm r = uncurryFields  r $ isAdmin' x0 cm
   where
    isAdmin' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "a") Address -> Web3 e (Either CallError Boolean)
    isAdmin' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: IsAdminFn)

--------------------------------------------------------------------------------
-- | GetNamespaceFn
--------------------------------------------------------------------------------


type GetNamespaceFn = Tagged (SProxy "getNamespace()") (Tuple0 )

getNamespace :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (DOne D4)))
getNamespace x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetNamespaceFn)

--------------------------------------------------------------------------------
-- | GetSponsorsNFn
--------------------------------------------------------------------------------


type GetSponsorsNFn = Tagged (SProxy "getSponsorsN(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getSponsorsN :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getSponsorsN x0 cm r = uncurryFields  r $ getSponsorsN' x0 cm
   where
    getSponsorsN' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getSponsorsN' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetSponsorsNFn)

--------------------------------------------------------------------------------
-- | GetSponsorFn
--------------------------------------------------------------------------------


type GetSponsorFn = Tagged (SProxy "getSponsor(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)))

getSponsor :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), sponsorN :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6))))
getSponsor x0 cm r = uncurryFields  r $ getSponsor' x0 cm
   where
    getSponsor' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "sponsorN") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6))))
    getSponsor' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetSponsorFn)

--------------------------------------------------------------------------------
-- | CurrAdminEpochFn
--------------------------------------------------------------------------------


type CurrAdminEpochFn = Tagged (SProxy "currAdminEpoch()") (Tuple0 )

currAdminEpoch :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
currAdminEpoch x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CurrAdminEpochFn)

--------------------------------------------------------------------------------
-- | GetAdminLogNFn
--------------------------------------------------------------------------------


type GetAdminLogNFn = Tagged (SProxy "getAdminLogN()") (Tuple0 )

getAdminLogN :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getAdminLogN x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetAdminLogNFn)

--------------------------------------------------------------------------------
-- | IncAdminEpochFn
--------------------------------------------------------------------------------


type IncAdminEpochFn = Tagged (SProxy "incAdminEpoch()") (Tuple0 )

incAdminEpoch :: forall e. TransactionOptions NoPay -> Web3 e HexString
incAdminEpoch x0 = sendTx x0 ((tagged $ Tuple0 ) :: IncAdminEpochFn)

--------------------------------------------------------------------------------
-- | GetVoteFn
--------------------------------------------------------------------------------


type GetVoteFn = Tagged (SProxy "getVote(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)))

getVote :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), voteId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) Address ByteString))
getVote x0 cm r = uncurryFields  r $ getVote' x0 cm
   where
    getVote' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "voteId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) Address ByteString))
    getVote' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetVoteFn)

--------------------------------------------------------------------------------
-- | SetAdminFn
--------------------------------------------------------------------------------


type SetAdminFn = Tagged (SProxy "setAdmin(address,bool)") (Tuple2 Address Boolean)

setAdmin :: forall e. TransactionOptions NoPay -> { a :: Address, _givePerms :: Boolean } -> Web3 e HexString
setAdmin x0 r = uncurryFields  r $ setAdmin' x0
   where
    setAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "a") Address -> Tagged (SProxy "_givePerms") Boolean -> Web3 e HexString
    setAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetAdminFn)

--------------------------------------------------------------------------------
-- | PayoutAllFn
--------------------------------------------------------------------------------


type PayoutAllFn = Tagged (SProxy "payoutAll()") (Tuple0 )

payoutAll :: forall e. TransactionOptions NoPay -> Web3 e HexString
payoutAll x0 = sendTx x0 ((tagged $ Tuple0 ) :: PayoutAllFn)

--------------------------------------------------------------------------------
-- | UpgradeMeFn
--------------------------------------------------------------------------------


type UpgradeMeFn = Tagged (SProxy "upgradeMe(address)") (Tuple1 Address)

upgradeMe :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
upgradeMe x0 r = uncurryFields  r $ upgradeMe' x0
   where
    upgradeMe' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    upgradeMe' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeMeFn)

--------------------------------------------------------------------------------
-- | AdminsDisabledForeverFn
--------------------------------------------------------------------------------


type AdminsDisabledForeverFn = Tagged (SProxy "adminsDisabledForever()") (Tuple0 )

adminsDisabledForever :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminsDisabledForever x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminsDisabledForeverFn)

--------------------------------------------------------------------------------
-- | SubmitProxyVoteFn
--------------------------------------------------------------------------------


type SubmitProxyVoteFn = Tagged (SProxy "submitProxyVote(bytes32[5],bytes)") (Tuple2 (Vector (DOne D5) (BytesN (D3 :& DOne D2))) ByteString)

submitProxyVote :: forall e. TransactionOptions NoPay -> { proxyReq :: (Vector (DOne D5) (BytesN (D3 :& DOne D2))), extra :: ByteString } -> Web3 e HexString
submitProxyVote x0 r = uncurryFields  r $ submitProxyVote' x0
   where
    submitProxyVote' :: TransactionOptions NoPay -> Tagged (SProxy "proxyReq") (Vector (DOne D5) (BytesN (D3 :& DOne D2))) -> Tagged (SProxy "extra") ByteString -> Web3 e HexString
    submitProxyVote' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SubmitProxyVoteFn)

--------------------------------------------------------------------------------
-- | GetTotalSponsorshipFn
--------------------------------------------------------------------------------


type GetTotalSponsorshipFn = Tagged (SProxy "getTotalSponsorship(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getTotalSponsorship :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getTotalSponsorship x0 cm r = uncurryFields  r $ getTotalSponsorship' x0 cm
   where
    getTotalSponsorship' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getTotalSponsorship' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetTotalSponsorshipFn)

--------------------------------------------------------------------------------
-- | SetPermissionsFn
--------------------------------------------------------------------------------


type SetPermissionsFn = Tagged (SProxy "setPermissions(address,bool)") (Tuple2 Address Boolean)

setPermissions :: forall e. TransactionOptions NoPay -> { e :: Address, _editPerms :: Boolean } -> Web3 e HexString
setPermissions x0 r = uncurryFields  r $ setPermissions' x0
   where
    setPermissions' :: TransactionOptions NoPay -> Tagged (SProxy "e") Address -> Tagged (SProxy "_editPerms") Boolean -> Web3 e HexString
    setPermissions' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetPermissionsFn)

--------------------------------------------------------------------------------
-- | AdminLockdownFn
--------------------------------------------------------------------------------


type AdminLockdownFn = Tagged (SProxy "adminLockdown()") (Tuple0 )

adminLockdown :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminLockdown x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminLockdownFn)

--------------------------------------------------------------------------------
-- | UpgradeMeAdminFn
--------------------------------------------------------------------------------


type UpgradeMeAdminFn = Tagged (SProxy "upgradeMeAdmin(address)") (Tuple1 Address)

upgradeMeAdmin :: forall e. TransactionOptions NoPay -> { newAdmin :: Address } -> Web3 e HexString
upgradeMeAdmin x0 r = uncurryFields  r $ upgradeMeAdmin' x0
   where
    upgradeMeAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    upgradeMeAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeMeAdminFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | GetDetailsFn
--------------------------------------------------------------------------------


type GetDetailsFn = Tagged (SProxy "getDetails(uint256,address)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address)

getDetails :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), voter :: Address } -> Web3 e (Either CallError (Tuple10 Boolean (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)) (UIntN (D1 :& DOne D6)) (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) Boolean Address (BytesN (D1 :& DOne D6))))
getDetails x0 cm r = uncurryFields  r $ getDetails' x0 cm
   where
    getDetails' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "voter") Address -> Web3 e (Either CallError (Tuple10 Boolean (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)) (UIntN (D1 :& DOne D6)) (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) Boolean Address (BytesN (D1 :& DOne D6))))
    getDetails' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDetailsFn)

--------------------------------------------------------------------------------
-- | RevealSeckeyFn
--------------------------------------------------------------------------------


type RevealSeckeyFn = Tagged (SProxy "revealSeckey(uint256,bytes32)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)))

revealSeckey :: forall e. TransactionOptions NoPay -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), sk :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
revealSeckey x0 r = uncurryFields  r $ revealSeckey' x0
   where
    revealSeckey' :: TransactionOptions NoPay -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "sk") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    revealSeckey' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: RevealSeckeyFn)

--------------------------------------------------------------------------------
-- | SetEndTimeFn
--------------------------------------------------------------------------------


type SetEndTimeFn = Tagged (SProxy "setEndTime(uint256,uint64)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D6 :& DOne D4)))

setEndTime :: forall e. TransactionOptions NoPay -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), newEndTime :: (UIntN (D6 :& DOne D4)) } -> Web3 e HexString
setEndTime x0 r = uncurryFields  r $ setEndTime' x0
   where
    setEndTime' :: TransactionOptions NoPay -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "newEndTime") (UIntN (D6 :& DOne D4)) -> Web3 e HexString
    setEndTime' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetEndTimeFn)

--------------------------------------------------------------------------------
-- | UpgradePermissionedSCFn
--------------------------------------------------------------------------------


type UpgradePermissionedSCFn = Tagged (SProxy "upgradePermissionedSC(address,address)") (Tuple2 Address Address)

upgradePermissionedSC :: forall e. TransactionOptions NoPay -> { oldSC :: Address, newSC :: Address } -> Web3 e HexString
upgradePermissionedSC x0 r = uncurryFields  r $ upgradePermissionedSC' x0
   where
    upgradePermissionedSC' :: TransactionOptions NoPay -> Tagged (SProxy "oldSC") Address -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    upgradePermissionedSC' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: UpgradePermissionedSCFn)

--------------------------------------------------------------------------------
-- | SponsorFn
--------------------------------------------------------------------------------


type SponsorFn = Tagged (SProxy "sponsor(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

sponsor :: forall e. TransactionOptions Wei -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
sponsor x0 r = uncurryFields  r $ sponsor' x0
   where
    sponsor' :: TransactionOptions Wei -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    sponsor' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SponsorFn)

--------------------------------------------------------------------------------
-- | GetSequenceNumberFn
--------------------------------------------------------------------------------


type GetSequenceNumberFn = Tagged (SProxy "getSequenceNumber(uint256,address)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address)

getSequenceNumber :: forall e. TransactionOptions NoPay -> ChainCursor -> { ballotId :: (UIntN (D2 :& D5 :& DOne D6)), voter :: Address } -> Web3 e (Either CallError (UIntN (D3 :& DOne D2)))
getSequenceNumber x0 cm r = uncurryFields  r $ getSequenceNumber' x0 cm
   where
    getSequenceNumber' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "voter") Address -> Web3 e (Either CallError (UIntN (D3 :& DOne D2)))
    getSequenceNumber' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetSequenceNumberFn)

--------------------------------------------------------------------------------
-- | GetBBLibVersionFn
--------------------------------------------------------------------------------


type GetBBLibVersionFn = Tagged (SProxy "getBBLibVersion()") (Tuple0 )

getBBLibVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBBLibVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBBLibVersionFn)

--------------------------------------------------------------------------------
-- | InitBallotFn
--------------------------------------------------------------------------------


type InitBallotFn = Tagged (SProxy "initBallot(bytes32,uint256,address,address,bytes24)") (Tuple5 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) Address Address (BytesN (D2 :& DOne D4)))

initBallot :: forall e. TransactionOptions NoPay -> { specHash :: (BytesN (D3 :& DOne D2)), packed :: (UIntN (D2 :& D5 :& DOne D6)), ix :: Address, bbAdmin :: Address, extraData :: (BytesN (D2 :& DOne D4)) } -> Web3 e HexString
initBallot x0 r = uncurryFields  r $ initBallot' x0
   where
    initBallot' :: TransactionOptions NoPay -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "ix") Address -> Tagged (SProxy "bbAdmin") Address -> Tagged (SProxy "extraData") (BytesN (D2 :& DOne D4)) -> Web3 e HexString
    initBallot' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: InitBallotFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | PayoutAll
--------------------------------------------------------------------------------


newtype PayoutAll = PayoutAll {payTo :: Address,value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePayoutAll :: Newtype PayoutAll _

instance eventFilterPayoutAll :: EventFilter PayoutAll where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e2644f8d6fd3207ea14ef6a361b94bee348c8e5834539376241010dbd2562472")]

instance indexedEventPayoutAll :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "payTo") Address) (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) PayoutAll where
  isAnonymous _ = false

derive instance genericPayoutAll :: Generic PayoutAll _

instance eventGenericPayoutAllShow :: Show PayoutAll where
	show = genericShow

instance eventGenericPayoutAlleq :: Eq PayoutAll where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionError
--------------------------------------------------------------------------------


newtype PermissionError = PermissionError {editAddr :: Address}

derive instance newtypePermissionError :: Newtype PermissionError _

instance eventFilterPermissionError :: EventFilter PermissionError where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ba1558d6c3ad0688fe6d3e0a7ee68da13b944fc53864b461b74c92e4a9654a3e")]

instance indexedEventPermissionError :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "editAddr") Address)) PermissionError where
  isAnonymous _ = false

derive instance genericPermissionError :: Generic PermissionError _

instance eventGenericPermissionErrorShow :: Show PermissionError where
	show = genericShow

instance eventGenericPermissionErroreq :: Eq PermissionError where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionGranted
--------------------------------------------------------------------------------


newtype PermissionGranted = PermissionGranted {editAddr :: Address}

derive instance newtypePermissionGranted :: Newtype PermissionGranted _

instance eventFilterPermissionGranted :: EventFilter PermissionGranted where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c1f0ea3cc21b72d778e7e9d433c419eabb16edce0afe4468769e055b2e6d49c6")]

instance indexedEventPermissionGranted :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "editAddr") Address)) PermissionGranted where
  isAnonymous _ = false

derive instance genericPermissionGranted :: Generic PermissionGranted _

instance eventGenericPermissionGrantedShow :: Show PermissionGranted where
	show = genericShow

instance eventGenericPermissionGrantedeq :: Eq PermissionGranted where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionRevoked
--------------------------------------------------------------------------------


newtype PermissionRevoked = PermissionRevoked {editAddr :: Address}

derive instance newtypePermissionRevoked :: Newtype PermissionRevoked _

instance eventFilterPermissionRevoked :: EventFilter PermissionRevoked where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3541f93cbae8c4be65491b824efe1570976e740b18c6aa441db5291f4de4c921")]

instance indexedEventPermissionRevoked :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "editAddr") Address)) PermissionRevoked where
  isAnonymous _ = false

derive instance genericPermissionRevoked :: Generic PermissionRevoked _

instance eventGenericPermissionRevokedShow :: Show PermissionRevoked where
	show = genericShow

instance eventGenericPermissionRevokedeq :: Eq PermissionRevoked where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionsUpgraded
--------------------------------------------------------------------------------


newtype PermissionsUpgraded = PermissionsUpgraded {oldSC :: Address,newSC :: Address}

derive instance newtypePermissionsUpgraded :: Newtype PermissionsUpgraded _

instance eventFilterPermissionsUpgraded :: EventFilter PermissionsUpgraded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "14e3af41624ed426a3e0e05e698f9abc5f7c5a80bab49a1b6f7ab4e534702b58")]

instance indexedEventPermissionsUpgraded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "oldSC") Address) (Tagged (SProxy "newSC") Address)) PermissionsUpgraded where
  isAnonymous _ = false

derive instance genericPermissionsUpgraded :: Generic PermissionsUpgraded _

instance eventGenericPermissionsUpgradedShow :: Show PermissionsUpgraded where
	show = genericShow

instance eventGenericPermissionsUpgradedeq :: Eq PermissionsUpgraded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SelfUpgrade
--------------------------------------------------------------------------------


newtype SelfUpgrade = SelfUpgrade {oldSC :: Address,newSC :: Address}

derive instance newtypeSelfUpgrade :: Newtype SelfUpgrade _

instance eventFilterSelfUpgrade :: EventFilter SelfUpgrade where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "4532cbbb9747736f93100911e83c51f9509459a759d4fe4f8a942688cce83c2a")]

instance indexedEventSelfUpgrade :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "oldSC") Address) (Tagged (SProxy "newSC") Address)) SelfUpgrade where
  isAnonymous _ = false

derive instance genericSelfUpgrade :: Generic SelfUpgrade _

instance eventGenericSelfUpgradeShow :: Show SelfUpgrade where
	show = genericShow

instance eventGenericSelfUpgradeeq :: Eq SelfUpgrade where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminLockdown
--------------------------------------------------------------------------------


newtype AdminLockdown = AdminLockdown {}

derive instance newtypeAdminLockdown :: Newtype AdminLockdown _

instance eventFilterAdminLockdown :: EventFilter AdminLockdown where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2fa084a3abd5513daa7f5bfb140cf0ae5d4e4bb7ec06479fe25956313701a205")]

instance indexedEventAdminLockdown :: IndexedEvent (Tuple0 ) (Tuple0 ) AdminLockdown where
  isAnonymous _ = false

derive instance genericAdminLockdown :: Generic AdminLockdown _

instance eventGenericAdminLockdownShow :: Show AdminLockdown where
	show = genericShow

instance eventGenericAdminLockdowneq :: Eq AdminLockdown where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminAdded
--------------------------------------------------------------------------------


newtype AdminAdded = AdminAdded {newAdmin :: Address}

derive instance newtypeAdminAdded :: Newtype AdminAdded _

instance eventFilterAdminAdded :: EventFilter AdminAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "44d6d25963f097ad14f29f06854a01f575648a1ef82f30e562ccd3889717e339"),Nothing]

instance indexedEventAdminAdded :: IndexedEvent (Tuple1 (Tagged (SProxy "newAdmin") Address)) (Tuple0 ) AdminAdded where
  isAnonymous _ = false

derive instance genericAdminAdded :: Generic AdminAdded _

instance eventGenericAdminAddedShow :: Show AdminAdded where
	show = genericShow

instance eventGenericAdminAddedeq :: Eq AdminAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminRemoved
--------------------------------------------------------------------------------


newtype AdminRemoved = AdminRemoved {oldAdmin :: Address}

derive instance newtypeAdminRemoved :: Newtype AdminRemoved _

instance eventFilterAdminRemoved :: EventFilter AdminRemoved where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a3b62bc36326052d97ea62d63c3d60308ed4c3ea8ac079dd8499f1e9c4f80c0f"),Nothing]

instance indexedEventAdminRemoved :: IndexedEvent (Tuple1 (Tagged (SProxy "oldAdmin") Address)) (Tuple0 ) AdminRemoved where
  isAnonymous _ = false

derive instance genericAdminRemoved :: Generic AdminRemoved _

instance eventGenericAdminRemovedShow :: Show AdminRemoved where
	show = genericShow

instance eventGenericAdminRemovedeq :: Eq AdminRemoved where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminEpochInc
--------------------------------------------------------------------------------


newtype AdminEpochInc = AdminEpochInc {}

derive instance newtypeAdminEpochInc :: Newtype AdminEpochInc _

instance eventFilterAdminEpochInc :: EventFilter AdminEpochInc where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c536428a6a2ea6a7cff457a274794564f9f6ce1cfcf4c0a53fadaa231b017d8a")]

instance indexedEventAdminEpochInc :: IndexedEvent (Tuple0 ) (Tuple0 ) AdminEpochInc where
  isAnonymous _ = false

derive instance genericAdminEpochInc :: Generic AdminEpochInc _

instance eventGenericAdminEpochIncShow :: Show AdminEpochInc where
	show = genericShow

instance eventGenericAdminEpochInceq :: Eq AdminEpochInc where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminDisabledForever
--------------------------------------------------------------------------------


newtype AdminDisabledForever = AdminDisabledForever {}

derive instance newtypeAdminDisabledForever :: Newtype AdminDisabledForever _

instance eventFilterAdminDisabledForever :: EventFilter AdminDisabledForever where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e6c1892f8d36012439015afa98d305e0aa27017e4042014c39690c8626d4a4a1")]

instance indexedEventAdminDisabledForever :: IndexedEvent (Tuple0 ) (Tuple0 ) AdminDisabledForever where
  isAnonymous _ = false

derive instance genericAdminDisabledForever :: Generic AdminDisabledForever _

instance eventGenericAdminDisabledForeverShow :: Show AdminDisabledForever where
	show = genericShow

instance eventGenericAdminDisabledForevereq :: Eq AdminDisabledForever where
	eq = genericEq

--------------------------------------------------------------------------------
-- | OwnerChanged
--------------------------------------------------------------------------------


newtype OwnerChanged = OwnerChanged {newOwner :: Address}

derive instance newtypeOwnerChanged :: Newtype OwnerChanged _

instance eventFilterOwnerChanged :: EventFilter OwnerChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a2ea9883a321a3e97b8266c2b078bfeec6d50c711ed71f874a90d500ae2eaf36")]

instance indexedEventOwnerChanged :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "newOwner") Address)) OwnerChanged where
  isAnonymous _ = false

derive instance genericOwnerChanged :: Generic OwnerChanged _

instance eventGenericOwnerChangedShow :: Show OwnerChanged where
	show = genericShow

instance eventGenericOwnerChangedeq :: Eq OwnerChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BallotCreatedWithID
--------------------------------------------------------------------------------


newtype BallotCreatedWithID = BallotCreatedWithID {ballotId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBallotCreatedWithID :: Newtype BallotCreatedWithID _

instance eventFilterBallotCreatedWithID :: EventFilter BallotCreatedWithID where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "20f1b9a21ee397f1c57261849e4492865559e3da426f13a27e9d3abefafb45ed")]

instance indexedEventBallotCreatedWithID :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)))) BallotCreatedWithID where
  isAnonymous _ = false

derive instance genericBallotCreatedWithID :: Generic BallotCreatedWithID _

instance eventGenericBallotCreatedWithIDShow :: Show BallotCreatedWithID where
	show = genericShow

instance eventGenericBallotCreatedWithIDeq :: Eq BallotCreatedWithID where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BBFarmInit
--------------------------------------------------------------------------------


newtype BBFarmInit = BBFarmInit {namespace :: (BytesN (DOne D4))}

derive instance newtypeBBFarmInit :: Newtype BBFarmInit _

instance eventFilterBBFarmInit :: EventFilter BBFarmInit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "9efcb9c0754671258cec21b6dce843609343e2240774fedbc3a062d6d79ed0f8")]

instance indexedEventBBFarmInit :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "namespace") (BytesN (DOne D4)))) BBFarmInit where
  isAnonymous _ = false

derive instance genericBBFarmInit :: Generic BBFarmInit _

instance eventGenericBBFarmInitShow :: Show BBFarmInit where
	show = genericShow

instance eventGenericBBFarmIniteq :: Eq BBFarmInit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Sponsorship
--------------------------------------------------------------------------------


newtype Sponsorship = Sponsorship {ballotId :: (UIntN (D2 :& D5 :& DOne D6)),value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSponsorship :: Newtype Sponsorship _

instance eventFilterSponsorship :: EventFilter Sponsorship where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ed34dd96c912079eb7961c72c52b776c5da4e532cd4dbbf12ce5178654775769")]

instance indexedEventSponsorship :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) Sponsorship where
  isAnonymous _ = false

derive instance genericSponsorship :: Generic Sponsorship _

instance eventGenericSponsorshipShow :: Show Sponsorship where
	show = genericShow

instance eventGenericSponsorshipeq :: Eq Sponsorship where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Vote
--------------------------------------------------------------------------------


newtype Vote = Vote {ballotId :: (UIntN (D2 :& D5 :& DOne D6)),vote :: (BytesN (D3 :& DOne D2)),voter :: Address,extra :: ByteString}

derive instance newtypeVote :: Newtype Vote _

instance eventFilterVote :: EventFilter Vote where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c36bca23f7acd356bf53dd9ba26e965997f89db6c92337e3960cbb2c5210199a"),Nothing]

instance indexedEventVote :: IndexedEvent (Tuple1 (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple3 (Tagged (SProxy "vote") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "voter") Address) (Tagged (SProxy "extra") ByteString)) Vote where
  isAnonymous _ = false

derive instance genericVote :: Generic Vote _

instance eventGenericVoteShow :: Show Vote where
	show = genericShow

instance eventGenericVoteeq :: Eq Vote where
	eq = genericEq