--------------------------------------------------------------------------------
-- | ENSIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.ENSIface where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D6, DOne, Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ResolverFn
--------------------------------------------------------------------------------


type ResolverFn = Tagged (SProxy "resolver(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

resolver :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
resolver x0 cm r = uncurryFields  r $ resolver' x0 cm
   where
    resolver' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    resolver' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: ResolverFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
owner x0 cm r = uncurryFields  r $ owner' x0 cm
   where
    owner' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    owner' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SetSubnodeOwnerFn
--------------------------------------------------------------------------------


type SetSubnodeOwnerFn = Tagged (SProxy "setSubnodeOwner(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address)

setSubnodeOwner :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), label :: (BytesN (D3 :& DOne D2)), owner :: Address } -> Web3 e HexString
setSubnodeOwner x0 r = uncurryFields  r $ setSubnodeOwner' x0
   where
    setSubnodeOwner' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "label") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "owner") Address -> Web3 e HexString
    setSubnodeOwner' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SetSubnodeOwnerFn)

--------------------------------------------------------------------------------
-- | SetTTLFn
--------------------------------------------------------------------------------


type SetTTLFn = Tagged (SProxy "setTTL(bytes32,uint64)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D6 :& DOne D4)))

setTTL :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), ttl :: (UIntN (D6 :& DOne D4)) } -> Web3 e HexString
setTTL x0 r = uncurryFields  r $ setTTL' x0
   where
    setTTL' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "ttl") (UIntN (D6 :& DOne D4)) -> Web3 e HexString
    setTTL' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetTTLFn)

--------------------------------------------------------------------------------
-- | TtlFn
--------------------------------------------------------------------------------


type TtlFn = Tagged (SProxy "ttl(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

ttl :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
ttl x0 cm r = uncurryFields  r $ ttl' x0 cm
   where
    ttl' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
    ttl' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: TtlFn)

--------------------------------------------------------------------------------
-- | SetResolverFn
--------------------------------------------------------------------------------


type SetResolverFn = Tagged (SProxy "setResolver(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setResolver :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), resolver :: Address } -> Web3 e HexString
setResolver x0 r = uncurryFields  r $ setResolver' x0
   where
    setResolver' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "resolver") Address -> Web3 e HexString
    setResolver' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetResolverFn)

--------------------------------------------------------------------------------
-- | SetOwnerFn
--------------------------------------------------------------------------------


type SetOwnerFn = Tagged (SProxy "setOwner(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setOwner :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), owner :: Address } -> Web3 e HexString
setOwner x0 r = uncurryFields  r $ setOwner' x0
   where
    setOwner' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "owner") Address -> Web3 e HexString
    setOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetOwnerFn)

--------------------------------------------------------------------------------
-- | NewOwner
--------------------------------------------------------------------------------


newtype NewOwner = NewOwner {node :: (BytesN (D3 :& DOne D2)),label :: (BytesN (D3 :& DOne D2)),owner :: Address}

derive instance newtypeNewOwner :: Newtype NewOwner _

instance eventFilterNewOwner :: EventFilter NewOwner where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ce0457fe73731f824cc272376169235128c118b49d344817417c6d108d155e82"),Nothing,Nothing]

instance indexedEventNewOwner :: IndexedEvent (Tuple2 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "label") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "owner") Address)) NewOwner where
  isAnonymous _ = false

derive instance genericNewOwner :: Generic NewOwner _

instance eventGenericNewOwnerShow :: Show NewOwner where
	show = genericShow

instance eventGenericNewOwnereq :: Eq NewOwner where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {node :: (BytesN (D3 :& DOne D2)),owner :: Address}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d4735d920b0f87494915f556dd9b54c8f309026070caea5c737245152564d266"),Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "owner") Address)) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
	show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
	eq = genericEq

--------------------------------------------------------------------------------
-- | NewResolver
--------------------------------------------------------------------------------


newtype NewResolver = NewResolver {node :: (BytesN (D3 :& DOne D2)),resolver :: Address}

derive instance newtypeNewResolver :: Newtype NewResolver _

instance eventFilterNewResolver :: EventFilter NewResolver where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "335721b01866dc23fbee8b6b2c7b1e14d6f05c28cd35a2c934239f94095602a0"),Nothing]

instance indexedEventNewResolver :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "resolver") Address)) NewResolver where
  isAnonymous _ = false

derive instance genericNewResolver :: Generic NewResolver _

instance eventGenericNewResolverShow :: Show NewResolver where
	show = genericShow

instance eventGenericNewResolvereq :: Eq NewResolver where
	eq = genericEq

--------------------------------------------------------------------------------
-- | NewTTL
--------------------------------------------------------------------------------


newtype NewTTL = NewTTL {node :: (BytesN (D3 :& DOne D2)),ttl :: (UIntN (D6 :& DOne D4))}

derive instance newtypeNewTTL :: Newtype NewTTL _

instance eventFilterNewTTL :: EventFilter NewTTL where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1d4f9bbfc9cab89d66e1a1562f2233ccbf1308cb4f63de2ead5787adddb8fa68"),Nothing]

instance indexedEventNewTTL :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "ttl") (UIntN (D6 :& DOne D4)))) NewTTL where
  isAnonymous _ = false

derive instance genericNewTTL :: Generic NewTTL _

instance eventGenericNewTTLShow :: Show NewTTL where
	show = genericShow

instance eventGenericNewTTLeq :: Eq NewTTL where
	eq = genericEq