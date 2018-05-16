--------------------------------------------------------------------------------
-- | IxPaymentsIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxPaymentsIface where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2, Tuple3(..), Tuple4, UIntN, class IndexedEvent, unTuple1)
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
-- | GetCommunityBallotCentsPriceFn
--------------------------------------------------------------------------------


type GetCommunityBallotCentsPriceFn = Tagged (SProxy "getCommunityBallotCentsPrice()") (Tuple0 )

getCommunityBallotCentsPrice :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getCommunityBallotCentsPrice x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetCommunityBallotCentsPriceFn)

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
-- | DowngradeToBasicFn
--------------------------------------------------------------------------------


type DowngradeToBasicFn = Tagged (SProxy "downgradeToBasic(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

downgradeToBasic :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
downgradeToBasic x0 r = uncurryFields  r $ downgradeToBasic' x0
   where
    downgradeToBasic' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    downgradeToBasic' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DowngradeToBasicFn)

--------------------------------------------------------------------------------
-- | SetPaymentEnabledFn
--------------------------------------------------------------------------------


type SetPaymentEnabledFn = Tagged (SProxy "setPaymentEnabled(bool)") (Tuple1 Boolean)

setPaymentEnabled :: forall e. TransactionOptions NoPay -> Boolean -> Web3 e HexString
setPaymentEnabled x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetPaymentEnabledFn)

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

upgradeMe :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
upgradeMe x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: UpgradeMeFn)

--------------------------------------------------------------------------------
-- | SetPayToFn
--------------------------------------------------------------------------------


type SetPayToFn = Tagged (SProxy "setPayTo(address)") (Tuple1 Address)

setPayTo :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
setPayTo x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetPayToFn)

--------------------------------------------------------------------------------
-- | SetCommunityBallotCentsPriceFn
--------------------------------------------------------------------------------


type SetCommunityBallotCentsPriceFn = Tagged (SProxy "setCommunityBallotCentsPrice(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setCommunityBallotCentsPrice :: forall e. TransactionOptions NoPay -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
setCommunityBallotCentsPrice x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetCommunityBallotCentsPriceFn)

--------------------------------------------------------------------------------
-- | GetPaymentLogNFn
--------------------------------------------------------------------------------


type GetPaymentLogNFn = Tagged (SProxy "getPaymentLogN()") (Tuple0 )

getPaymentLogN :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getPaymentLogN x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentLogNFn)

--------------------------------------------------------------------------------
-- | GetPremiumMultiplierFn
--------------------------------------------------------------------------------


type GetPremiumMultiplierFn = Tagged (SProxy "getPremiumMultiplier()") (Tuple0 )

getPremiumMultiplier :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (DOne D8)))
getPremiumMultiplier x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | GetWeiPerCentFn
--------------------------------------------------------------------------------


type GetWeiPerCentFn = Tagged (SProxy "getWeiPerCent()") (Tuple0 )

getWeiPerCent :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getWeiPerCent x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetWeiPerCentFn)

--------------------------------------------------------------------------------
-- | GetPaymentEnabledFn
--------------------------------------------------------------------------------


type GetPaymentEnabledFn = Tagged (SProxy "getPaymentEnabled()") (Tuple0 )

getPaymentEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
getPaymentEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentEnabledFn)

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

setWeiPerCent :: forall e. TransactionOptions NoPay -> { weiPerCent :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setWeiPerCent x0 r = uncurryFields  r $ setWeiPerCent' x0
   where
    setWeiPerCent' :: TransactionOptions NoPay -> Tagged (SProxy "weiPerCent") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setWeiPerCent' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetWeiPerCentFn)

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

setPremiumMultiplier :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (DOne D8)) } -> Web3 e HexString
setPremiumMultiplier x0 r = uncurryFields  r $ setPremiumMultiplier' x0
   where
    setPremiumMultiplier' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (DOne D8)) -> Web3 e HexString
    setPremiumMultiplier' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | GetBasicCentsPricePer30DaysFn
--------------------------------------------------------------------------------


type GetBasicCentsPricePer30DaysFn = Tagged (SProxy "getBasicCentsPricePer30Days()") (Tuple0 )

getBasicCentsPricePer30Days :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBasicCentsPricePer30Days x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBasicCentsPricePer30DaysFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getPayTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPayToFn)

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
-- | GetPremiumPricePer30DaysFn
--------------------------------------------------------------------------------


type GetPremiumPricePer30DaysFn = Tagged (SProxy "getPremiumPricePer30Days()") (Tuple0 )

getPremiumPricePer30Days :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getPremiumPricePer30Days x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumPricePer30DaysFn)

--------------------------------------------------------------------------------
-- | PaymentEnabled
--------------------------------------------------------------------------------


newtype PaymentEnabled = PaymentEnabled {feeEnabled :: Boolean}

derive instance newtypePaymentEnabled :: Newtype PaymentEnabled _

instance eventFilterPaymentEnabled :: EventFilter PaymentEnabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ed53661a07b1eecfd9ce68c3067068f29dc710c7144b27617faed07dbec21b90")]

instance indexedEventPaymentEnabled :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "feeEnabled") Boolean)) PaymentEnabled where
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