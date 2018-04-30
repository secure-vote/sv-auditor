--------------------------------------------------------------------------------
-- | SVIndexPaymentSettings
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVIndexPaymentSettings where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
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
-- | GetBasicPricePerSecondFn
--------------------------------------------------------------------------------


type GetBasicPricePerSecondFn = Tagged (SProxy "getBasicPricePerSecond()") (Tuple0 )

getBasicPricePerSecond :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBasicPricePerSecond x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBasicPricePerSecondFn)

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
-- | HasPermissionsFn
--------------------------------------------------------------------------------


type HasPermissionsFn = Tagged (SProxy "hasPermissions(address)") (Tuple1 Address)

hasPermissions :: forall e. TransactionOptions NoPay -> ChainCursor -> { a :: Address } -> Web3 e (Either CallError Boolean)
hasPermissions x0 cm r = uncurryFields  r $ hasPermissions' x0 cm
   where
    hasPermissions' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "a") Address -> Web3 e (Either CallError Boolean)
    hasPermissions' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: HasPermissionsFn)

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
-- | AccountInGoodStandingFn
--------------------------------------------------------------------------------


type AccountInGoodStandingFn = Tagged (SProxy "accountInGoodStanding(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

accountInGoodStanding :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
accountInGoodStanding x0 cm r = uncurryFields  r $ accountInGoodStanding' x0 cm
   where
    accountInGoodStanding' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    accountInGoodStanding' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: AccountInGoodStandingFn)

--------------------------------------------------------------------------------
-- | SetBasicPricePerSecondFn
--------------------------------------------------------------------------------


type SetBasicPricePerSecondFn = Tagged (SProxy "setBasicPricePerSecond(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setBasicPricePerSecond :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setBasicPricePerSecond x0 r = uncurryFields  r $ setBasicPricePerSecond' x0
   where
    setBasicPricePerSecond' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setBasicPricePerSecond' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetBasicPricePerSecondFn)

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
-- | SetPaymentEnabledFn
--------------------------------------------------------------------------------


type SetPaymentEnabledFn = Tagged (SProxy "setPaymentEnabled(bool)") (Tuple1 Boolean)

setPaymentEnabled :: forall e. TransactionOptions NoPay -> { _enabled :: Boolean } -> Web3 e HexString
setPaymentEnabled x0 r = uncurryFields  r $ setPaymentEnabled' x0
   where
    setPaymentEnabled' :: TransactionOptions NoPay -> Tagged (SProxy "_enabled") Boolean -> Web3 e HexString
    setPaymentEnabled' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPaymentEnabledFn)

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
-- | SetCommunityBallotFeeFn
--------------------------------------------------------------------------------


type SetCommunityBallotFeeFn = Tagged (SProxy "setCommunityBallotFee(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setCommunityBallotFee :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setCommunityBallotFee x0 r = uncurryFields  r $ setCommunityBallotFee' x0
   where
    setCommunityBallotFee' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setCommunityBallotFee' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetCommunityBallotFeeFn)

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
-- | GetPremiumPricePerSecondFn
--------------------------------------------------------------------------------


type GetPremiumPricePerSecondFn = Tagged (SProxy "getPremiumPricePerSecond()") (Tuple0 )

getPremiumPricePerSecond :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getPremiumPricePerSecond x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumPricePerSecondFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | GetPaymentEnabledFn
--------------------------------------------------------------------------------


type GetPaymentEnabledFn = Tagged (SProxy "getPaymentEnabled()") (Tuple0 )

getPaymentEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
getPaymentEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentEnabledFn)

--------------------------------------------------------------------------------
-- | GetCommunityBallotFeeFn
--------------------------------------------------------------------------------


type GetCommunityBallotFeeFn = Tagged (SProxy "getCommunityBallotFee()") (Tuple0 )

getCommunityBallotFee :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getCommunityBallotFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetCommunityBallotFeeFn)

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
-- | PayToFn
--------------------------------------------------------------------------------


type PayToFn = Tagged (SProxy "payTo()") (Tuple0 )

payTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
payTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PayToFn)

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
-- | SetPremiumMultiplierFn
--------------------------------------------------------------------------------


type SetPremiumMultiplierFn = Tagged (SProxy "setPremiumMultiplier(uint8)") (Tuple1 (UIntN (DOne D8)))

setPremiumMultiplier :: forall e. TransactionOptions NoPay -> { m :: (UIntN (DOne D8)) } -> Web3 e HexString
setPremiumMultiplier x0 r = uncurryFields  r $ setPremiumMultiplier' x0
   where
    setPremiumMultiplier' :: TransactionOptions NoPay -> Tagged (SProxy "m") (UIntN (DOne D8)) -> Web3 e HexString
    setPremiumMultiplier' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getPayTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPayToFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)



--------------------------------------------------------------------------------
-- | PaymentEnabled
--------------------------------------------------------------------------------


newtype PaymentEnabled = PaymentEnabled {_feeEnabled :: Boolean}

derive instance newtypePaymentEnabled :: Newtype PaymentEnabled _

instance eventFilterPaymentEnabled :: EventFilter PaymentEnabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ed53661a07b1eecfd9ce68c3067068f29dc710c7144b27617faed07dbec21b90")]

instance indexedEventPaymentEnabled :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "_feeEnabled") Boolean)) PaymentEnabled where
  isAnonymous _ = false

derive instance genericPaymentEnabled :: Generic PaymentEnabled _

instance eventGenericPaymentEnabledShow :: Show PaymentEnabled where
	show = genericShow

instance eventGenericPaymentEnabledeq :: Eq PaymentEnabled where
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
-- | SetBasicPricePerSecond
--------------------------------------------------------------------------------


newtype SetBasicPricePerSecond = SetBasicPricePerSecond {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetBasicPricePerSecond :: Newtype SetBasicPricePerSecond _

instance eventFilterSetBasicPricePerSecond :: EventFilter SetBasicPricePerSecond where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "efea4c4e4fb58e5a0876fd60ff268b8d1d4699fda811f9980c568ec8a4f1ba73")]

instance indexedEventSetBasicPricePerSecond :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetBasicPricePerSecond where
  isAnonymous _ = false

derive instance genericSetBasicPricePerSecond :: Generic SetBasicPricePerSecond _

instance eventGenericSetBasicPricePerSecondShow :: Show SetBasicPricePerSecond where
	show = genericShow

instance eventGenericSetBasicPricePerSecondeq :: Eq SetBasicPricePerSecond where
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
-- | Error
--------------------------------------------------------------------------------


newtype Error = Error {code :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeError :: Newtype Error _

instance eventFilterError :: EventFilter Error where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2e36a7093f25f22bd4cbdeb6040174c3ba4c5fe8f1abc04e7c3c48f26c7413e0")]

instance indexedEventError :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "code") (UIntN (D2 :& D5 :& DOne D6)))) Error where
  isAnonymous _ = false

derive instance genericError :: Generic Error _

instance eventGenericErrorShow :: Show Error where
	show = genericShow

instance eventGenericErroreq :: Eq Error where
	eq = genericEq