--------------------------------------------------------------------------------
-- | SVLightIndex
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVLightIndex where

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
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), UIntN, class IndexedEvent, unTuple1)
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
-- | AddCategoryFn
--------------------------------------------------------------------------------


type AddCategoryFn = Tagged (SProxy "addCategory(bytes32,bytes32,bool,uint256)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6)))

addCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), categoryName :: (BytesN (D3 :& DOne D2)), hasParent :: Boolean, parent :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
addCategory x0 r = uncurryFields  r $ addCategory' x0
   where
    addCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryName") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hasParent") Boolean -> Tagged (SProxy "parent") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    addCategory' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: AddCategoryFn)

--------------------------------------------------------------------------------
-- | BackendFn
--------------------------------------------------------------------------------


type BackendFn = Tagged (SProxy "backend()") (Tuple0 )

backend :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
backend x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BackendFn)

--------------------------------------------------------------------------------
-- | AdminPxFactoryFn
--------------------------------------------------------------------------------


type AdminPxFactoryFn = Tagged (SProxy "adminPxFactory()") (Tuple0 )

adminPxFactory :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
adminPxFactory x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminPxFactoryFn)

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
-- | SetPaymentBackendFn
--------------------------------------------------------------------------------


type SetPaymentBackendFn = Tagged (SProxy "setPaymentBackend(address)") (Tuple1 Address)

setPaymentBackend :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
setPaymentBackend x0 r = uncurryFields  r $ setPaymentBackend' x0
   where
    setPaymentBackend' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    setPaymentBackend' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPaymentBackendFn)

--------------------------------------------------------------------------------
-- | GetDemocInfoFn
--------------------------------------------------------------------------------


type GetDemocInfoFn = Tagged (SProxy "getDemocInfo(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDemocInfo :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& DOne D6))))
getDemocInfo x0 cm r = uncurryFields  r $ getDemocInfo' x0 cm
   where
    getDemocInfo' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& DOne D6))))
    getDemocInfo' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocInfoFn)

--------------------------------------------------------------------------------
-- | GetDAdminFn
--------------------------------------------------------------------------------


type GetDAdminFn = Tagged (SProxy "getDAdmin(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDAdmin :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
getDAdmin x0 cm r = uncurryFields  r $ getDAdmin' x0 cm
   where
    getDAdmin' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    getDAdmin' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDAdminFn)

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
-- | DoUpgradeFn
--------------------------------------------------------------------------------


type DoUpgradeFn = Tagged (SProxy "doUpgrade(address)") (Tuple1 Address)

doUpgrade :: forall e. TransactionOptions NoPay -> { nextSC :: Address } -> Web3 e HexString
doUpgrade x0 r = uncurryFields  r $ doUpgrade' x0
   where
    doUpgrade' :: TransactionOptions NoPay -> Tagged (SProxy "nextSC") Address -> Web3 e HexString
    doUpgrade' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DoUpgradeFn)

--------------------------------------------------------------------------------
-- | GetDemocHashFn
--------------------------------------------------------------------------------


type GetDemocHashFn = Tagged (SProxy "getDemocHash(bytes13)") (Tuple1 (BytesN (D1 :& DOne D3)))

getDemocHash :: forall e. TransactionOptions NoPay -> ChainCursor -> { prefix :: (BytesN (D1 :& DOne D3)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getDemocHash x0 cm r = uncurryFields  r $ getDemocHash' x0 cm
   where
    getDemocHash' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "prefix") (BytesN (D1 :& DOne D3)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getDemocHash' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocHashFn)

--------------------------------------------------------------------------------
-- | GetDemocCategoryFn
--------------------------------------------------------------------------------


type GetDemocCategoryFn = Tagged (SProxy "getDemocCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDemocCategory :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), categoryId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
getDemocCategory x0 cm r = uncurryFields  r $ getDemocCategory' x0 cm
   where
    getDemocCategory' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
    getDemocCategory' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDemocCategoryFn)

--------------------------------------------------------------------------------
-- | NDemocsFn
--------------------------------------------------------------------------------


type NDemocsFn = Tagged (SProxy "nDemocs()") (Tuple0 )

nDemocs :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nDemocs x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NDemocsFn)

--------------------------------------------------------------------------------
-- | DeployBallotFn
--------------------------------------------------------------------------------


type DeployBallotFn = Tagged (SProxy "deployBallot(bytes32,bytes32,bytes32,uint128,uint16)") (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D1 :& D2 :& DOne D8)) (UIntN (D1 :& DOne D6)))

deployBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)), specHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), packedTimes :: (UIntN (D1 :& D2 :& DOne D8)), _submissionBits :: (UIntN (D1 :& DOne D6)) } -> Web3 e HexString
deployBallot x0 r = uncurryFields  r $ deployBallot' x0
   where
    deployBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packedTimes") (UIntN (D1 :& D2 :& DOne D8)) -> Tagged (SProxy "_submissionBits") (UIntN (D1 :& DOne D6)) -> Web3 e HexString
    deployBallot' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: DeployBallotFn)

--------------------------------------------------------------------------------
-- | PaymentSettingsFn
--------------------------------------------------------------------------------


type PaymentSettingsFn = Tagged (SProxy "paymentSettings()") (Tuple0 )

paymentSettings :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
paymentSettings x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PaymentSettingsFn)

--------------------------------------------------------------------------------
-- | GetNthBallotFn
--------------------------------------------------------------------------------


type GetNthBallotFn = Tagged (SProxy "getNthBallot(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getNthBallot :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4))))
getNthBallot x0 cm r = uncurryFields  r $ getNthBallot' x0 cm
   where
    getNthBallot' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4))))
    getNthBallot' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetNthBallotFn)

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
-- | AddBallotFn
--------------------------------------------------------------------------------


type AddBallotFn = Tagged (SProxy "addBallot(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address)

addBallot :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), bb :: Address } -> Web3 e HexString
addBallot x0 r = uncurryFields  r $ addBallot' x0
   where
    addBallot' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "bb") Address -> Web3 e HexString
    addBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: AddBallotFn)

--------------------------------------------------------------------------------
-- | GetUpgradePointerFn
--------------------------------------------------------------------------------


type GetUpgradePointerFn = Tagged (SProxy "getUpgradePointer()") (Tuple0 )

getUpgradePointer :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getUpgradePointer x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetUpgradePointerFn)

--------------------------------------------------------------------------------
-- | GetCommunityBallotFeeFn
--------------------------------------------------------------------------------


type GetCommunityBallotFeeFn = Tagged (SProxy "getCommunityBallotFee()") (Tuple0 )

getCommunityBallotFee :: forall e. TransactionOptions NoPay -> Web3 e HexString
getCommunityBallotFee x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetCommunityBallotFeeFn)

--------------------------------------------------------------------------------
-- | BbFactoryFn
--------------------------------------------------------------------------------


type BbFactoryFn = Tagged (SProxy "bbFactory()") (Tuple0 )

bbFactory :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
bbFactory x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BbFactoryFn)

--------------------------------------------------------------------------------
-- | GetBallotAddrFn
--------------------------------------------------------------------------------


type GetBallotAddrFn = Tagged (SProxy "getBallotAddr(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getBallotAddr :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getBallotAddr x0 cm r = uncurryFields  r $ getBallotAddr' x0 cm
   where
    getBallotAddr' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getBallotAddr' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetBallotAddrFn)

--------------------------------------------------------------------------------
-- | GetDemocNCategoriesFn
--------------------------------------------------------------------------------


type GetDemocNCategoriesFn = Tagged (SProxy "getDemocNCategories(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDemocNCategories :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDemocNCategories x0 cm r = uncurryFields  r $ getDemocNCategories' x0 cm
   where
    getDemocNCategories' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDemocNCategories' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocNCategoriesFn)

--------------------------------------------------------------------------------
-- | NBallotsFn
--------------------------------------------------------------------------------


type NBallotsFn = Tagged (SProxy "nBallots(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

nBallots :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nBallots x0 cm r = uncurryFields  r $ nBallots' x0 cm
   where
    nBallots' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    nBallots' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: NBallotsFn)

--------------------------------------------------------------------------------
-- | SetBackendFn
--------------------------------------------------------------------------------


type SetBackendFn = Tagged (SProxy "setBackend(address)") (Tuple1 Address)

setBackend :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
setBackend x0 r = uncurryFields  r $ setBackend' x0
   where
    setBackend' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    setBackend' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetBackendFn)

--------------------------------------------------------------------------------
-- | SetDAdminFn
--------------------------------------------------------------------------------


type SetDAdminFn = Tagged (SProxy "setDAdmin(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDAdmin :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newAdmin :: Address } -> Web3 e HexString
setDAdmin x0 r = uncurryFields  r $ setDAdmin' x0
   where
    setDAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    setDAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDAdminFn)

--------------------------------------------------------------------------------
-- | DeprecateCategoryFn
--------------------------------------------------------------------------------


type DeprecateCategoryFn = Tagged (SProxy "deprecateCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

deprecateCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), categoryId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
deprecateCategory x0 r = uncurryFields  r $ deprecateCategory' x0
   where
    deprecateCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    deprecateCategory' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DeprecateCategoryFn)

--------------------------------------------------------------------------------
-- | EnsPxFn
--------------------------------------------------------------------------------


type EnsPxFn = Tagged (SProxy "ensPx()") (Tuple0 )

ensPx :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
ensPx x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EnsPxFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> Web3 e HexString
getPayTo x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetPayToFn)

--------------------------------------------------------------------------------
-- | InitDemocFn
--------------------------------------------------------------------------------


type InitDemocFn = Tagged (SProxy "initDemoc(string)") (Tuple1 String)

initDemoc :: forall e. TransactionOptions Wei -> { democName :: String } -> Web3 e HexString
initDemoc x0 r = uncurryFields  r $ initDemoc' x0
   where
    initDemoc' :: TransactionOptions Wei -> Tagged (SProxy "democName") String -> Web3 e HexString
    initDemoc' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: InitDemocFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address,address,address,address)") (Tuple5 Address Address Address Address Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _backend :: Address, _payBackend :: Address, _pxFactory :: Address, _bbFactory :: Address, _ensPx :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_backend") Address -> Tagged (SProxy "_payBackend") Address -> Tagged (SProxy "_pxFactory") Address -> Tagged (SProxy "_bbFactory") Address -> Tagged (SProxy "_ensPx") Address -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 y5 y6 = deployContract y0 bc' ((tagged $ Tuple5 (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 ) (untagged y6 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | PaymentMade
--------------------------------------------------------------------------------


newtype PaymentMade = PaymentMade {valAndRemainder :: (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))}

derive instance newtypePaymentMade :: Newtype PaymentMade _

instance eventFilterPaymentMade :: EventFilter PaymentMade where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a10b8a9911b680d93c2cbb993f5f5a4921bb52ebcb7926bf777c3831a5a1f30b")]

instance indexedEventPaymentMade :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "valAndRemainder") (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))))) PaymentMade where
  isAnonymous _ = false

derive instance genericPaymentMade :: Generic PaymentMade _

instance eventGenericPaymentMadeShow :: Show PaymentMade where
	show = genericShow

instance eventGenericPaymentMadeeq :: Eq PaymentMade where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocAdded
--------------------------------------------------------------------------------


newtype DemocAdded = DemocAdded {democHash :: (BytesN (D3 :& DOne D2)),admin :: Address}

derive instance newtypeDemocAdded :: Newtype DemocAdded _

instance eventFilterDemocAdded :: EventFilter DemocAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a3d25223b03bc5efb59ee8ede174c158d0a82a774249ed58a8cd3d4d818bd74a")]

instance indexedEventDemocAdded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "admin") Address)) DemocAdded where
  isAnonymous _ = false

derive instance genericDemocAdded :: Generic DemocAdded _

instance eventGenericDemocAddedShow :: Show DemocAdded where
	show = genericShow

instance eventGenericDemocAddedeq :: Eq DemocAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BallotAdded
--------------------------------------------------------------------------------


newtype BallotAdded = BallotAdded {democHash :: (BytesN (D3 :& DOne D2)),id :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBallotAdded :: Newtype BallotAdded _

instance eventFilterBallotAdded :: EventFilter BallotAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "dc273ca230283938fefd191bc6bf22e7d6e445e3a6619722c57f72b69389776a")]

instance indexedEventBallotAdded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)))) BallotAdded where
  isAnonymous _ = false

derive instance genericBallotAdded :: Generic BallotAdded _

instance eventGenericBallotAddedShow :: Show BallotAdded where
	show = genericShow

instance eventGenericBallotAddedeq :: Eq BallotAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PaymentTooLow
--------------------------------------------------------------------------------


newtype PaymentTooLow = PaymentTooLow {msgValue :: (UIntN (D2 :& D5 :& DOne D6)),feeReq :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePaymentTooLow :: Newtype PaymentTooLow _

instance eventFilterPaymentTooLow :: EventFilter PaymentTooLow where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0cbe2bf2897282a407ced6374d0e65b5da5eeef9c81b0b809bc3dda776302142")]

instance indexedEventPaymentTooLow :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "msgValue") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "feeReq") (UIntN (D2 :& D5 :& DOne D6)))) PaymentTooLow where
  isAnonymous _ = false

derive instance genericPaymentTooLow :: Generic PaymentTooLow _

instance eventGenericPaymentTooLowShow :: Show PaymentTooLow where
	show = genericShow

instance eventGenericPaymentTooLoweq :: Eq PaymentTooLow where
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