--------------------------------------------------------------------------------
-- | SVPayments
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVPayments where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | PayForDemocracyFn
--------------------------------------------------------------------------------


type PayForDemocracyFn = Tagged (SProxy "payForDemocracy(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

payForDemocracy :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
payForDemocracy x0 r = uncurryFields  r $ payForDemocracy' x0
   where
    payForDemocracy' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    payForDemocracy' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: PayForDemocracyFn)

--------------------------------------------------------------------------------
-- | GiveTimeToDemocFn
--------------------------------------------------------------------------------


type GiveTimeToDemocFn = Tagged (SProxy "giveTimeToDemoc(bytes32,uint256,bytes32)") (Tuple3 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)))

giveTimeToDemoc :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), additionalSeconds :: (UIntN (D2 :& D5 :& DOne D6)), ref :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
giveTimeToDemoc x0 r = uncurryFields  r $ giveTimeToDemoc' x0
   where
    giveTimeToDemoc' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "additionalSeconds") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "ref") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    giveTimeToDemoc' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: GiveTimeToDemocFn)

--------------------------------------------------------------------------------
-- | SetBasicCentsPricePer30DaysFn
--------------------------------------------------------------------------------


type SetBasicCentsPricePer30DaysFn = Tagged (SProxy "setBasicCentsPricePer30Days(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setBasicCentsPricePer30Days :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setBasicCentsPricePer30Days x0 r = uncurryFields  r $ setBasicCentsPricePer30Days' x0
   where
    setBasicCentsPricePer30Days' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setBasicCentsPricePer30Days' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetBasicCentsPricePer30DaysFn)

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
-- | GetDenyPremiumFn
--------------------------------------------------------------------------------


type GetDenyPremiumFn = Tagged (SProxy "getDenyPremium(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDenyPremium :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
getDenyPremium x0 cm r = uncurryFields  r $ getDenyPremium' x0 cm
   where
    getDenyPremium' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    getDenyPremium' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDenyPremiumFn)

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
-- | EmergencySetOwnerFn
--------------------------------------------------------------------------------


type EmergencySetOwnerFn = Tagged (SProxy "emergencySetOwner(address)") (Tuple1 Address)

emergencySetOwner :: forall e. TransactionOptions NoPay -> { newOwner :: Address } -> Web3 e HexString
emergencySetOwner x0 r = uncurryFields  r $ emergencySetOwner' x0
   where
    emergencySetOwner' :: TransactionOptions NoPay -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    emergencySetOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: EmergencySetOwnerFn)

--------------------------------------------------------------------------------
-- | MinorEditsAddrFn
--------------------------------------------------------------------------------


type MinorEditsAddrFn = Tagged (SProxy "minorEditsAddr()") (Tuple0 )

minorEditsAddr :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
minorEditsAddr x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: MinorEditsAddrFn)

--------------------------------------------------------------------------------
-- | ReclaimTokenFn
--------------------------------------------------------------------------------


type ReclaimTokenFn = Tagged (SProxy "reclaimToken(address)") (Tuple1 Address)

reclaimToken :: forall e. TransactionOptions NoPay -> { token :: Address } -> Web3 e HexString
reclaimToken x0 r = uncurryFields  r $ reclaimToken' x0
   where
    reclaimToken' :: TransactionOptions NoPay -> Tagged (SProxy "token") Address -> Web3 e HexString
    reclaimToken' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: ReclaimTokenFn)

--------------------------------------------------------------------------------
-- | CentsToWeiFn
--------------------------------------------------------------------------------


type CentsToWeiFn = Tagged (SProxy "centsToWei(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

centsToWei :: forall e. TransactionOptions NoPay -> ChainCursor -> { c :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
centsToWei x0 cm r = uncurryFields  r $ centsToWei' x0 cm
   where
    centsToWei' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "c") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    centsToWei' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: CentsToWeiFn)

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
-- | SetDenyPremiumFn
--------------------------------------------------------------------------------


type SetDenyPremiumFn = Tagged (SProxy "setDenyPremium(bytes32,bool)") (Tuple2 (BytesN (D3 :& DOne D2)) Boolean)

setDenyPremium :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), isPremiumDenied :: Boolean } -> Web3 e HexString
setDenyPremium x0 r = uncurryFields  r $ setDenyPremium' x0
   where
    setDenyPremium' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "isPremiumDenied") Boolean -> Web3 e HexString
    setDenyPremium' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDenyPremiumFn)

--------------------------------------------------------------------------------
-- | AccountInGoodStandingFn
--------------------------------------------------------------------------------


type AccountInGoodStandingFn = Tagged (SProxy "accountInGoodStanding(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

accountInGoodStanding :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
accountInGoodStanding x0 cm r = uncurryFields  r $ accountInGoodStanding' x0 cm
   where
    accountInGoodStanding' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    accountInGoodStanding' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: AccountInGoodStandingFn)

--------------------------------------------------------------------------------
-- | SetMinorEditsAddrFn
--------------------------------------------------------------------------------


type SetMinorEditsAddrFn = Tagged (SProxy "setMinorEditsAddr(address)") (Tuple1 Address)

setMinorEditsAddr :: forall e. TransactionOptions NoPay -> { a :: Address } -> Web3 e HexString
setMinorEditsAddr x0 r = uncurryFields  r $ setMinorEditsAddr' x0
   where
    setMinorEditsAddr' :: TransactionOptions NoPay -> Tagged (SProxy "a") Address -> Web3 e HexString
    setMinorEditsAddr' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetMinorEditsAddrFn)

--------------------------------------------------------------------------------
-- | CurrAdminEpochFn
--------------------------------------------------------------------------------


type CurrAdminEpochFn = Tagged (SProxy "currAdminEpoch()") (Tuple0 )

currAdminEpoch :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
currAdminEpoch x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CurrAdminEpochFn)

--------------------------------------------------------------------------------
-- | DowngradeToBasicFn
--------------------------------------------------------------------------------


type DowngradeToBasicFn = Tagged (SProxy "downgradeToBasic(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

downgradeToBasic :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
downgradeToBasic x0 r = uncurryFields  r $ downgradeToBasic' x0
   where
    downgradeToBasic' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    downgradeToBasic' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DowngradeToBasicFn)

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
-- | GetBasicBallotsPer30DaysFn
--------------------------------------------------------------------------------


type GetBasicBallotsPer30DaysFn = Tagged (SProxy "getBasicBallotsPer30Days()") (Tuple0 )

getBasicBallotsPer30Days :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBasicBallotsPer30Days x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBasicBallotsPer30DaysFn)

--------------------------------------------------------------------------------
-- | AdminsDisabledForeverFn
--------------------------------------------------------------------------------


type AdminsDisabledForeverFn = Tagged (SProxy "adminsDisabledForever()") (Tuple0 )

adminsDisabledForever :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminsDisabledForever x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminsDisabledForeverFn)

--------------------------------------------------------------------------------
-- | GetMinWeiForDInitFn
--------------------------------------------------------------------------------


type GetMinWeiForDInitFn = Tagged (SProxy "getMinWeiForDInit()") (Tuple0 )

getMinWeiForDInit :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getMinWeiForDInit x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetMinWeiForDInitFn)

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
-- | SetPayToFn
--------------------------------------------------------------------------------


type SetPayToFn = Tagged (SProxy "setPayTo(address)") (Tuple1 Address)

setPayTo :: forall e. TransactionOptions NoPay -> { newPayTo :: Address } -> Web3 e HexString
setPayTo x0 r = uncurryFields  r $ setPayTo' x0
   where
    setPayTo' :: TransactionOptions NoPay -> Tagged (SProxy "newPayTo") Address -> Web3 e HexString
    setPayTo' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPayToFn)

--------------------------------------------------------------------------------
-- | GetFreeExtensionFn
--------------------------------------------------------------------------------


type GetFreeExtensionFn = Tagged (SProxy "getFreeExtension(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getFreeExtension :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
getFreeExtension x0 cm r = uncurryFields  r $ getFreeExtension' x0 cm
   where
    getFreeExtension' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    getFreeExtension' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetFreeExtensionFn)

--------------------------------------------------------------------------------
-- | GetPaymentLogNFn
--------------------------------------------------------------------------------


type GetPaymentLogNFn = Tagged (SProxy "getPaymentLogN()") (Tuple0 )

getPaymentLogN :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getPaymentLogN x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentLogNFn)

--------------------------------------------------------------------------------
-- | WeiToCentsFn
--------------------------------------------------------------------------------


type WeiToCentsFn = Tagged (SProxy "weiToCents(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

weiToCents :: forall e. TransactionOptions NoPay -> ChainCursor -> { w :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
weiToCents x0 cm r = uncurryFields  r $ weiToCents' x0 cm
   where
    weiToCents' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "w") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    weiToCents' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: WeiToCentsFn)

--------------------------------------------------------------------------------
-- | EmergencyAdminFn
--------------------------------------------------------------------------------


type EmergencyAdminFn = Tagged (SProxy "emergencyAdmin()") (Tuple0 )

emergencyAdmin :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
emergencyAdmin x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EmergencyAdminFn)

--------------------------------------------------------------------------------
-- | GetPremiumStatusFn
--------------------------------------------------------------------------------


type GetPremiumStatusFn = Tagged (SProxy "getPremiumStatus(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getPremiumStatus :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
getPremiumStatus x0 cm r = uncurryFields  r $ getPremiumStatus' x0 cm
   where
    getPremiumStatus' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    getPremiumStatus' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetPremiumStatusFn)

--------------------------------------------------------------------------------
-- | GetPremiumMultiplierFn
--------------------------------------------------------------------------------


type GetPremiumMultiplierFn = Tagged (SProxy "getPremiumMultiplier()") (Tuple0 )

getPremiumMultiplier :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (DOne D8)))
getPremiumMultiplier x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | AdminLockdownFn
--------------------------------------------------------------------------------


type AdminLockdownFn = Tagged (SProxy "adminLockdown()") (Tuple0 )

adminLockdown :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminLockdown x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminLockdownFn)

--------------------------------------------------------------------------------
-- | GetWeiPerCentFn
--------------------------------------------------------------------------------


type GetWeiPerCentFn = Tagged (SProxy "getWeiPerCent()") (Tuple0 )

getWeiPerCent :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getWeiPerCent x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetWeiPerCentFn)

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
-- | SetBasicBallotsPer30DaysFn
--------------------------------------------------------------------------------


type SetBasicBallotsPer30DaysFn = Tagged (SProxy "setBasicBallotsPer30Days(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setBasicBallotsPer30Days :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setBasicBallotsPer30Days x0 r = uncurryFields  r $ setBasicBallotsPer30Days' x0
   where
    setBasicBallotsPer30Days' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setBasicBallotsPer30Days' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetBasicBallotsPer30DaysFn)

--------------------------------------------------------------------------------
-- | GetBasicExtraBallotFeeWeiFn
--------------------------------------------------------------------------------


type GetBasicExtraBallotFeeWeiFn = Tagged (SProxy "getBasicExtraBallotFeeWei()") (Tuple0 )

getBasicExtraBallotFeeWei :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBasicExtraBallotFeeWei x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBasicExtraBallotFeeWeiFn)

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
-- | SetFreeExtensionFn
--------------------------------------------------------------------------------


type SetFreeExtensionFn = Tagged (SProxy "setFreeExtension(bytes32,bool)") (Tuple2 (BytesN (D3 :& DOne D2)) Boolean)

setFreeExtension :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), hasFreeExt :: Boolean } -> Web3 e HexString
setFreeExtension x0 r = uncurryFields  r $ setFreeExtension' x0
   where
    setFreeExtension' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hasFreeExt") Boolean -> Web3 e HexString
    setFreeExtension' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetFreeExtensionFn)

--------------------------------------------------------------------------------
-- | UpgradeToPremiumFn
--------------------------------------------------------------------------------


type UpgradeToPremiumFn = Tagged (SProxy "upgradeToPremium(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

upgradeToPremium :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
upgradeToPremium x0 r = uncurryFields  r $ upgradeToPremium' x0
   where
    upgradeToPremium' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    upgradeToPremium' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeToPremiumFn)

--------------------------------------------------------------------------------
-- | SetWeiPerCentFn
--------------------------------------------------------------------------------


type SetWeiPerCentFn = Tagged (SProxy "setWeiPerCent(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setWeiPerCent :: forall e. TransactionOptions NoPay -> { wpc :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setWeiPerCent x0 r = uncurryFields  r $ setWeiPerCent' x0
   where
    setWeiPerCent' :: TransactionOptions NoPay -> Tagged (SProxy "wpc") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setWeiPerCent' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetWeiPerCentFn)

--------------------------------------------------------------------------------
-- | GetAccountFn
--------------------------------------------------------------------------------


type GetAccountFn = Tagged (SProxy "getAccount(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getAccount :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple4 Boolean (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Boolean))
getAccount x0 cm r = uncurryFields  r $ getAccount' x0 cm
   where
    getAccount' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple4 Boolean (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Boolean))
    getAccount' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetAccountFn)

--------------------------------------------------------------------------------
-- | GetUsdEthExchangeRateFn
--------------------------------------------------------------------------------


type GetUsdEthExchangeRateFn = Tagged (SProxy "getUsdEthExchangeRate()") (Tuple0 )

getUsdEthExchangeRate :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getUsdEthExchangeRate x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetUsdEthExchangeRateFn)

--------------------------------------------------------------------------------
-- | SetPremiumMultiplierFn
--------------------------------------------------------------------------------


type SetPremiumMultiplierFn = Tagged (SProxy "setPremiumMultiplier(uint8)") (Tuple1 (UIntN (DOne D8)))

setPremiumMultiplier :: forall e. TransactionOptions NoPay -> { m :: (UIntN (DOne D8)) } -> Web3 e HexString
setPremiumMultiplier x0 r = uncurryFields  r $ setPremiumMultiplier' x0
   where
    setPremiumMultiplier' :: TransactionOptions NoPay -> Tagged (SProxy "m") (UIntN (DOne D8)) -> Web3 e HexString
    setPremiumMultiplier' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | GetBasicCentsPricePer30DaysFn
--------------------------------------------------------------------------------


type GetBasicCentsPricePer30DaysFn = Tagged (SProxy "getBasicCentsPricePer30Days()") (Tuple0 )

getBasicCentsPricePer30Days :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBasicCentsPricePer30Days x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBasicCentsPricePer30DaysFn)

--------------------------------------------------------------------------------
-- | GetPremiumCentsPricePer30DaysFn
--------------------------------------------------------------------------------


type GetPremiumCentsPricePer30DaysFn = Tagged (SProxy "getPremiumCentsPricePer30Days()") (Tuple0 )

getPremiumCentsPricePer30Days :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getPremiumCentsPricePer30Days x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumCentsPricePer30DaysFn)

--------------------------------------------------------------------------------
-- | GetSecondsRemainingFn
--------------------------------------------------------------------------------


type GetSecondsRemainingFn = Tagged (SProxy "getSecondsRemaining(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getSecondsRemaining :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getSecondsRemaining x0 cm r = uncurryFields  r $ getSecondsRemaining' x0 cm
   where
    getSecondsRemaining' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getSecondsRemaining' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetSecondsRemainingFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getPayTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPayToFn)

--------------------------------------------------------------------------------
-- | SetMinWeiForDInitFn
--------------------------------------------------------------------------------


type SetMinWeiForDInitFn = Tagged (SProxy "setMinWeiForDInit(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setMinWeiForDInit :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setMinWeiForDInit x0 r = uncurryFields  r $ setMinWeiForDInit' x0
   where
    setMinWeiForDInit' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setMinWeiForDInit' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetMinWeiForDInitFn)

--------------------------------------------------------------------------------
-- | WeiBuysHowManySecondsFn
--------------------------------------------------------------------------------


type WeiBuysHowManySecondsFn = Tagged (SProxy "weiBuysHowManySeconds(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

weiBuysHowManySeconds :: forall e. TransactionOptions NoPay -> ChainCursor -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
weiBuysHowManySeconds x0 cm r = uncurryFields  r $ weiBuysHowManySeconds' x0 cm
   where
    weiBuysHowManySeconds' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    weiBuysHowManySeconds' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: WeiBuysHowManySecondsFn)

--------------------------------------------------------------------------------
-- | GetPaymentLogFn
--------------------------------------------------------------------------------


type GetPaymentLogFn = Tagged (SProxy "getPaymentLog(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getPaymentLog :: forall e. TransactionOptions NoPay -> ChainCursor -> { n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
getPaymentLog x0 cm r = uncurryFields  r $ getPaymentLog' x0 cm
   where
    getPaymentLog' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
    getPaymentLog' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetPaymentLogFn)

--------------------------------------------------------------------------------
-- | DoFreeExtensionFn
--------------------------------------------------------------------------------


type DoFreeExtensionFn = Tagged (SProxy "doFreeExtension(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

doFreeExtension :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
doFreeExtension x0 r = uncurryFields  r $ doFreeExtension' x0
   where
    doFreeExtension' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    doFreeExtension' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DoFreeExtensionFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _emergencyAdmin :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_emergencyAdmin") Address -> Web3 e HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 (untagged y2 )) :: ConstructorFn)



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
-- | UpgradedToPremium
--------------------------------------------------------------------------------


newtype UpgradedToPremium = UpgradedToPremium {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeUpgradedToPremium :: Newtype UpgradedToPremium _

instance eventFilterUpgradedToPremium :: EventFilter UpgradedToPremium where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b957c6a4668e76eb2e541f54eff529717f8070dba0ef7813dd8ab28cbbf0f702"),Nothing]

instance indexedEventUpgradedToPremium :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) UpgradedToPremium where
  isAnonymous _ = false

derive instance genericUpgradedToPremium :: Generic UpgradedToPremium _

instance eventGenericUpgradedToPremiumShow :: Show UpgradedToPremium where
	show = genericShow

instance eventGenericUpgradedToPremiumeq :: Eq UpgradedToPremium where
	eq = genericEq

--------------------------------------------------------------------------------
-- | GrantedAccountTime
--------------------------------------------------------------------------------


newtype GrantedAccountTime = GrantedAccountTime {democHash :: (BytesN (D3 :& DOne D2)),additionalSeconds :: (UIntN (D2 :& D5 :& DOne D6)),ref :: (BytesN (D3 :& DOne D2))}

derive instance newtypeGrantedAccountTime :: Newtype GrantedAccountTime _

instance eventFilterGrantedAccountTime :: EventFilter GrantedAccountTime where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "662d736f3a0c8f3b0906b173689606e590f214abef4ae9bde801e1b6f4059f4d"),Nothing]

instance indexedEventGrantedAccountTime :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple2 (Tagged (SProxy "additionalSeconds") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "ref") (BytesN (D3 :& DOne D2)))) GrantedAccountTime where
  isAnonymous _ = false

derive instance genericGrantedAccountTime :: Generic GrantedAccountTime _

instance eventGenericGrantedAccountTimeShow :: Show GrantedAccountTime where
	show = genericShow

instance eventGenericGrantedAccountTimeeq :: Eq GrantedAccountTime where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AccountPayment
--------------------------------------------------------------------------------


newtype AccountPayment = AccountPayment {democHash :: (BytesN (D3 :& DOne D2)),additionalSeconds :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAccountPayment :: Newtype AccountPayment _

instance eventFilterAccountPayment :: EventFilter AccountPayment where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e6ad0535314a8031f23d28b2c0945848329bf43d9d12471ce4f5f99f66406576"),Nothing]

instance indexedEventAccountPayment :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "additionalSeconds") (UIntN (D2 :& D5 :& DOne D6)))) AccountPayment where
  isAnonymous _ = false

derive instance genericAccountPayment :: Generic AccountPayment _

instance eventGenericAccountPaymentShow :: Show AccountPayment where
	show = genericShow

instance eventGenericAccountPaymenteq :: Eq AccountPayment where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetCommunityBallotFee
--------------------------------------------------------------------------------


newtype SetCommunityBallotFee = SetCommunityBallotFee {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetCommunityBallotFee :: Newtype SetCommunityBallotFee _

instance eventFilterSetCommunityBallotFee :: EventFilter SetCommunityBallotFee where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "527bf3f500392f0f845f9c3c2b3a6b253d7d34472e90e894b25eed6e7dd79b7f")]

instance indexedEventSetCommunityBallotFee :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetCommunityBallotFee where
  isAnonymous _ = false

derive instance genericSetCommunityBallotFee :: Generic SetCommunityBallotFee _

instance eventGenericSetCommunityBallotFeeShow :: Show SetCommunityBallotFee where
	show = genericShow

instance eventGenericSetCommunityBallotFeeeq :: Eq SetCommunityBallotFee where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetBasicCentsPricePer30Days
--------------------------------------------------------------------------------


newtype SetBasicCentsPricePer30Days = SetBasicCentsPricePer30Days {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetBasicCentsPricePer30Days :: Newtype SetBasicCentsPricePer30Days _

instance eventFilterSetBasicCentsPricePer30Days :: EventFilter SetBasicCentsPricePer30Days where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "696e5c6fec16a11a25b2139177dc42dbfe83df4fd078f5a7f13b6db57792d1a1")]

instance indexedEventSetBasicCentsPricePer30Days :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetBasicCentsPricePer30Days where
  isAnonymous _ = false

derive instance genericSetBasicCentsPricePer30Days :: Generic SetBasicCentsPricePer30Days _

instance eventGenericSetBasicCentsPricePer30DaysShow :: Show SetBasicCentsPricePer30Days where
	show = genericShow

instance eventGenericSetBasicCentsPricePer30Dayseq :: Eq SetBasicCentsPricePer30Days where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetPremiumMultiplier
--------------------------------------------------------------------------------


newtype SetPremiumMultiplier = SetPremiumMultiplier {multiplier :: (UIntN (DOne D8))}

derive instance newtypeSetPremiumMultiplier :: Newtype SetPremiumMultiplier _

instance eventFilterSetPremiumMultiplier :: EventFilter SetPremiumMultiplier where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "dd5e68e86e7460257caaf7a2276fa63562e9a906a39802c4f37699f5736a72f2")]

instance indexedEventSetPremiumMultiplier :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "multiplier") (UIntN (DOne D8)))) SetPremiumMultiplier where
  isAnonymous _ = false

derive instance genericSetPremiumMultiplier :: Generic SetPremiumMultiplier _

instance eventGenericSetPremiumMultiplierShow :: Show SetPremiumMultiplier where
	show = genericShow

instance eventGenericSetPremiumMultipliereq :: Eq SetPremiumMultiplier where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DowngradeToBasic
--------------------------------------------------------------------------------


newtype DowngradeToBasic = DowngradeToBasic {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeDowngradeToBasic :: Newtype DowngradeToBasic _

instance eventFilterDowngradeToBasic :: EventFilter DowngradeToBasic where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "48f962cb175066eb5227bf2d28122d1f0861b74cd6951e6542a6f0283b6a30b7"),Nothing]

instance indexedEventDowngradeToBasic :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) DowngradeToBasic where
  isAnonymous _ = false

derive instance genericDowngradeToBasic :: Generic DowngradeToBasic _

instance eventGenericDowngradeToBasicShow :: Show DowngradeToBasic where
	show = genericShow

instance eventGenericDowngradeToBasiceq :: Eq DowngradeToBasic where
	eq = genericEq

--------------------------------------------------------------------------------
-- | UpgradeToPremium
--------------------------------------------------------------------------------


newtype UpgradeToPremium = UpgradeToPremium {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeUpgradeToPremium :: Newtype UpgradeToPremium _

instance eventFilterUpgradeToPremium :: EventFilter UpgradeToPremium where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ac98838319e78f6dc944f503ff87fffb262f5a9a6360e961a802837072b81e0f"),Nothing]

instance indexedEventUpgradeToPremium :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) UpgradeToPremium where
  isAnonymous _ = false

derive instance genericUpgradeToPremium :: Generic UpgradeToPremium _

instance eventGenericUpgradeToPremiumShow :: Show UpgradeToPremium where
	show = genericShow

instance eventGenericUpgradeToPremiumeq :: Eq UpgradeToPremium where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetExchangeRate
--------------------------------------------------------------------------------


newtype SetExchangeRate = SetExchangeRate {weiPerCent :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetExchangeRate :: Newtype SetExchangeRate _

instance eventFilterSetExchangeRate :: EventFilter SetExchangeRate where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1d5de90e7c5b244ac5797698b15fe80a92524d933dafd79e001daf844555fb1c")]

instance indexedEventSetExchangeRate :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "weiPerCent") (UIntN (D2 :& D5 :& DOne D6)))) SetExchangeRate where
  isAnonymous _ = false

derive instance genericSetExchangeRate :: Generic SetExchangeRate _

instance eventGenericSetExchangeRateShow :: Show SetExchangeRate where
	show = genericShow

instance eventGenericSetExchangeRateeq :: Eq SetExchangeRate where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FreeExtension
--------------------------------------------------------------------------------


newtype FreeExtension = FreeExtension {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeFreeExtension :: Newtype FreeExtension _

instance eventFilterFreeExtension :: EventFilter FreeExtension where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "09664427434c2b4309000ea03b5e378fa25c30acbe0a04fdeff36061303f8e7e")]

instance indexedEventFreeExtension :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) FreeExtension where
  isAnonymous _ = false

derive instance genericFreeExtension :: Generic FreeExtension _

instance eventGenericFreeExtensionShow :: Show FreeExtension where
	show = genericShow

instance eventGenericFreeExtensioneq :: Eq FreeExtension where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetBallotsPer30Days
--------------------------------------------------------------------------------


newtype SetBallotsPer30Days = SetBallotsPer30Days {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetBallotsPer30Days :: Newtype SetBallotsPer30Days _

instance eventFilterSetBallotsPer30Days :: EventFilter SetBallotsPer30Days where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "cfa56694fbda1b84c5d6c63fe8f6874c83c94f2da04231dd0d54e07cb542e62f")]

instance indexedEventSetBallotsPer30Days :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetBallotsPer30Days where
  isAnonymous _ = false

derive instance genericSetBallotsPer30Days :: Generic SetBallotsPer30Days _

instance eventGenericSetBallotsPer30DaysShow :: Show SetBallotsPer30Days where
	show = genericShow

instance eventGenericSetBallotsPer30Dayseq :: Eq SetBallotsPer30Days where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetFreeExtension
--------------------------------------------------------------------------------


newtype SetFreeExtension = SetFreeExtension {democHash :: (BytesN (D3 :& DOne D2)),hasFreeExt :: Boolean}

derive instance newtypeSetFreeExtension :: Newtype SetFreeExtension _

instance eventFilterSetFreeExtension :: EventFilter SetFreeExtension where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "de965928e65e33ccd6559d861e5483e90512b32455de3d5eec039b9a5b93f8e4")]

instance indexedEventSetFreeExtension :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "hasFreeExt") Boolean)) SetFreeExtension where
  isAnonymous _ = false

derive instance genericSetFreeExtension :: Generic SetFreeExtension _

instance eventGenericSetFreeExtensionShow :: Show SetFreeExtension where
	show = genericShow

instance eventGenericSetFreeExtensioneq :: Eq SetFreeExtension where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetDenyPremium
--------------------------------------------------------------------------------


newtype SetDenyPremium = SetDenyPremium {democHash :: (BytesN (D3 :& DOne D2)),isPremiumDenied :: Boolean}

derive instance newtypeSetDenyPremium :: Newtype SetDenyPremium _

instance eventFilterSetDenyPremium :: EventFilter SetDenyPremium where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "50935952ae272746cf38a1cb4aa7c93d3e39e60cdc4a761ef55a4e69366abe7d")]

instance indexedEventSetDenyPremium :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "isPremiumDenied") Boolean)) SetDenyPremium where
  isAnonymous _ = false

derive instance genericSetDenyPremium :: Generic SetDenyPremium _

instance eventGenericSetDenyPremiumShow :: Show SetDenyPremium where
	show = genericShow

instance eventGenericSetDenyPremiumeq :: Eq SetDenyPremium where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetPayTo
--------------------------------------------------------------------------------


newtype SetPayTo = SetPayTo {payTo :: Address}

derive instance newtypeSetPayTo :: Newtype SetPayTo _

instance eventFilterSetPayTo :: EventFilter SetPayTo where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "bea0769aba15b443dd3170dc4bc7b94123881e42d6ed3d31a2654daf43b05851")]

instance indexedEventSetPayTo :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "payTo") Address)) SetPayTo where
  isAnonymous _ = false

derive instance genericSetPayTo :: Generic SetPayTo _

instance eventGenericSetPayToShow :: Show SetPayTo where
	show = genericShow

instance eventGenericSetPayToeq :: Eq SetPayTo where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetMinorEditsAddr
--------------------------------------------------------------------------------


newtype SetMinorEditsAddr = SetMinorEditsAddr {minorEditsAddr :: Address}

derive instance newtypeSetMinorEditsAddr :: Newtype SetMinorEditsAddr _

instance eventFilterSetMinorEditsAddr :: EventFilter SetMinorEditsAddr where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d72f7e1ee158c792883f68eb79af23661439288dae047b5a0f1fb6c8cd884973")]

instance indexedEventSetMinorEditsAddr :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "minorEditsAddr") Address)) SetMinorEditsAddr where
  isAnonymous _ = false

derive instance genericSetMinorEditsAddr :: Generic SetMinorEditsAddr _

instance eventGenericSetMinorEditsAddrShow :: Show SetMinorEditsAddr where
	show = genericShow

instance eventGenericSetMinorEditsAddreq :: Eq SetMinorEditsAddr where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetMinWeiForDInit
--------------------------------------------------------------------------------


newtype SetMinWeiForDInit = SetMinWeiForDInit {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetMinWeiForDInit :: Newtype SetMinWeiForDInit _

instance eventFilterSetMinWeiForDInit :: EventFilter SetMinWeiForDInit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "134bc3fbd032443af0856b9e13b4146103106e96c9c095d10415247f5369c4cf")]

instance indexedEventSetMinWeiForDInit :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetMinWeiForDInit where
  isAnonymous _ = false

derive instance genericSetMinWeiForDInit :: Generic SetMinWeiForDInit _

instance eventGenericSetMinWeiForDInitShow :: Show SetMinWeiForDInit where
	show = genericShow

instance eventGenericSetMinWeiForDIniteq :: Eq SetMinWeiForDInit where
	eq = genericEq