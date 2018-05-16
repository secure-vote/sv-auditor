--------------------------------------------------------------------------------
-- | SVLightAdminProxy
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVLightAdminProxy where

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
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D2, D3, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | AddNewAdminFn
--------------------------------------------------------------------------------


type AddNewAdminFn = Tagged (SProxy "addNewAdmin(address)") (Tuple1 Address)

addNewAdmin :: forall e. TransactionOptions NoPay -> { newAdmin :: Address } -> Web3 e HexString
addNewAdmin x0 r = uncurryFields  r $ addNewAdmin' x0
   where
    addNewAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    addNewAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: AddNewAdminFn)

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
-- | RemoveAdminFn
--------------------------------------------------------------------------------


type RemoveAdminFn = Tagged (SProxy "removeAdmin(address)") (Tuple1 Address)

removeAdmin :: forall e. TransactionOptions NoPay -> { oldAdmin :: Address } -> Web3 e HexString
removeAdmin x0 r = uncurryFields  r $ removeAdmin' x0
   where
    removeAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "oldAdmin") Address -> Web3 e HexString
    removeAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RemoveAdminFn)

--------------------------------------------------------------------------------
-- | FwdPaymentFn
--------------------------------------------------------------------------------


type FwdPaymentFn = Tagged (SProxy "fwdPayment(address)") (Tuple1 Address)

fwdPayment :: forall e. TransactionOptions Wei -> { toAddr :: Address } -> Web3 e HexString
fwdPayment x0 r = uncurryFields  r $ fwdPayment' x0
   where
    fwdPayment' :: TransactionOptions Wei -> Tagged (SProxy "toAddr") Address -> Web3 e HexString
    fwdPayment' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: FwdPaymentFn)

--------------------------------------------------------------------------------
-- | DemocHashFn
--------------------------------------------------------------------------------


type DemocHashFn = Tagged (SProxy "democHash()") (Tuple0 )

democHash :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
democHash x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DemocHashFn)

--------------------------------------------------------------------------------
-- | AdminsFn
--------------------------------------------------------------------------------


type AdminsFn = Tagged (SProxy "admins(address)") (Tuple1 Address)

admins :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
admins x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AdminsFn)

--------------------------------------------------------------------------------
-- | SetOwnerAsAdminFn
--------------------------------------------------------------------------------


type SetOwnerAsAdminFn = Tagged (SProxy "setOwnerAsAdmin()") (Tuple0 )

setOwnerAsAdmin :: forall e. TransactionOptions NoPay -> Web3 e HexString
setOwnerAsAdmin x0 = sendTx x0 ((tagged $ Tuple0 ) :: SetOwnerAsAdminFn)

--------------------------------------------------------------------------------
-- | ProxyVersionFn
--------------------------------------------------------------------------------


type ProxyVersionFn = Tagged (SProxy "proxyVersion()") (Tuple0 )

proxyVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
proxyVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ProxyVersionFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | FnT_forwardToFn
--------------------------------------------------------------------------------


type FnT_forwardToFn = Tagged (SProxy "_forwardTo()") (Tuple0 )

_forwardTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
_forwardTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: FnT_forwardToFn)

--------------------------------------------------------------------------------
-- | CommunityBallotsEnabledFn
--------------------------------------------------------------------------------


type CommunityBallotsEnabledFn = Tagged (SProxy "communityBallotsEnabled()") (Tuple0 )

communityBallotsEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
communityBallotsEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CommunityBallotsEnabledFn)

--------------------------------------------------------------------------------
-- | FwdPaymentAndDataFn
--------------------------------------------------------------------------------


type FwdPaymentAndDataFn = Tagged (SProxy "fwdPaymentAndData(address,bytes)") (Tuple2 Address ByteString)

fwdPaymentAndData :: forall e. TransactionOptions Wei -> { toAddr :: Address, data :: ByteString } -> Web3 e HexString
fwdPaymentAndData x0 r = uncurryFields  r $ fwdPaymentAndData' x0
   where
    fwdPaymentAndData' :: TransactionOptions Wei -> Tagged (SProxy "toAddr") Address -> Tagged (SProxy "data") ByteString -> Web3 e HexString
    fwdPaymentAndData' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: FwdPaymentAndDataFn)

--------------------------------------------------------------------------------
-- | ListAllAdminsFn
--------------------------------------------------------------------------------


type ListAllAdminsFn = Tagged (SProxy "listAllAdmins()") (Tuple0 )

listAllAdmins :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (Array Address))
listAllAdmins x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ListAllAdminsFn)

--------------------------------------------------------------------------------
-- | IsProxyContractFn
--------------------------------------------------------------------------------


type IsProxyContractFn = Tagged (SProxy "isProxyContract()") (Tuple0 )

isProxyContract :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
isProxyContract x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsProxyContractFn)

--------------------------------------------------------------------------------
-- | DeployCommunityBallotFn
--------------------------------------------------------------------------------


type DeployCommunityBallotFn = Tagged (SProxy "deployCommunityBallot(bytes32,bytes32,uint256)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

deployCommunityBallot :: forall e. TransactionOptions Wei -> { specHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), _packed :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
deployCommunityBallot x0 r = uncurryFields  r $ deployCommunityBallot' x0
   where
    deployCommunityBallot' :: TransactionOptions Wei -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "_packed") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    deployCommunityBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DeployCommunityBallotFn)

--------------------------------------------------------------------------------
-- | SetCommunityBallotStatusFn
--------------------------------------------------------------------------------


type SetCommunityBallotStatusFn = Tagged (SProxy "setCommunityBallotStatus(bool)") (Tuple1 Boolean)

setCommunityBallotStatus :: forall e. TransactionOptions NoPay -> { isEnabled :: Boolean } -> Web3 e HexString
setCommunityBallotStatus x0 r = uncurryFields  r $ setCommunityBallotStatus' x0
   where
    setCommunityBallotStatus' :: TransactionOptions NoPay -> Tagged (SProxy "isEnabled") Boolean -> Web3 e HexString
    setCommunityBallotStatus' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetCommunityBallotStatusFn)

--------------------------------------------------------------------------------
-- | FwdDataFn
--------------------------------------------------------------------------------


type FwdDataFn = Tagged (SProxy "fwdData(address,bytes)") (Tuple2 Address ByteString)

fwdData :: forall e. TransactionOptions NoPay -> { toAddr :: Address, data :: ByteString } -> Web3 e HexString
fwdData x0 r = uncurryFields  r $ fwdData' x0
   where
    fwdData' :: TransactionOptions NoPay -> Tagged (SProxy "toAddr") Address -> Tagged (SProxy "data") ByteString -> Web3 e HexString
    fwdData' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: FwdDataFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(bytes32,address,address)") (Tuple3 (BytesN (D3 :& DOne D2)) Address Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _democHash :: (BytesN (D3 :& DOne D2)), initAdmin :: Address, _fwdTo :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "initAdmin") Address -> Tagged (SProxy "_fwdTo") Address -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 = deployContract y0 bc' ((tagged $ Tuple3 (untagged y2 ) (untagged y3 ) (untagged y4 )) :: ConstructorFn)



--------------------------------------------------------------------------------
-- | AddedAdminToPx
--------------------------------------------------------------------------------


newtype AddedAdminToPx = AddedAdminToPx {newAdmin :: Address}

derive instance newtypeAddedAdminToPx :: Newtype AddedAdminToPx _

instance eventFilterAddedAdminToPx :: EventFilter AddedAdminToPx where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d720fe884ee1a75e662c66abb294d93820d9d3dc6b6aa1513b0b3d70dc8579d3")]

instance indexedEventAddedAdminToPx :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "newAdmin") Address)) AddedAdminToPx where
  isAnonymous _ = false

derive instance genericAddedAdminToPx :: Generic AddedAdminToPx _

instance eventGenericAddedAdminToPxShow :: Show AddedAdminToPx where
	show = genericShow

instance eventGenericAddedAdminToPxeq :: Eq AddedAdminToPx where
	eq = genericEq

--------------------------------------------------------------------------------
-- | RemovedAdmin
--------------------------------------------------------------------------------


newtype RemovedAdmin = RemovedAdmin {oldAdmin :: Address}

derive instance newtypeRemovedAdmin :: Newtype RemovedAdmin _

instance eventFilterRemovedAdmin :: EventFilter RemovedAdmin where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3137a7fedbfedb7895dfa5a6812a4566dae9ded68d00aeec13c6ed1fa84a1c80")]

instance indexedEventRemovedAdmin :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "oldAdmin") Address)) RemovedAdmin where
  isAnonymous _ = false

derive instance genericRemovedAdmin :: Generic RemovedAdmin _

instance eventGenericRemovedAdminShow :: Show RemovedAdmin where
	show = genericShow

instance eventGenericRemovedAdmineq :: Eq RemovedAdmin where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FailedToFwdCall
--------------------------------------------------------------------------------


newtype FailedToFwdCall = FailedToFwdCall {value :: (UIntN (D2 :& D5 :& DOne D6)),data :: ByteString}

derive instance newtypeFailedToFwdCall :: Newtype FailedToFwdCall _

instance eventFilterFailedToFwdCall :: EventFilter FailedToFwdCall where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "437f2cb7c77e7b805eb0ee703a363f5e968c01f2102fc542ba4068f5bbebf7d2")]

instance indexedEventFailedToFwdCall :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "data") ByteString)) FailedToFwdCall where
  isAnonymous _ = false

derive instance genericFailedToFwdCall :: Generic FailedToFwdCall _

instance eventGenericFailedToFwdCallShow :: Show FailedToFwdCall where
	show = genericShow

instance eventGenericFailedToFwdCalleq :: Eq FailedToFwdCall where
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