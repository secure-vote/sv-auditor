--------------------------------------------------------------------------------
-- | PublicResolver
--------------------------------------------------------------------------------

module SecureVote.Contracts.PublicResolver where

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
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D2, D3, D4, D5, D6, DOne, Tuple0, Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | SupportsInterfaceFn
--------------------------------------------------------------------------------


type SupportsInterfaceFn = Tagged (SProxy "supportsInterface(bytes4)") (Tuple1 (BytesN (DOne D4)))

supportsInterface :: forall e. TransactionOptions NoPay -> ChainCursor -> { interfaceID :: (BytesN (DOne D4)) } -> Web3 e (Either CallError Boolean)
supportsInterface x0 cm r = uncurryFields  r $ supportsInterface' x0 cm
   where
    supportsInterface' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "interfaceID") (BytesN (DOne D4)) -> Web3 e (Either CallError Boolean)
    supportsInterface' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: SupportsInterfaceFn)

--------------------------------------------------------------------------------
-- | SetTextFn
--------------------------------------------------------------------------------


type SetTextFn = Tagged (SProxy "setText(bytes32,string,string)") (Tuple3 (BytesN (D3 :& DOne D2)) String String)

setText :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), key :: String, value :: String } -> Web3 e HexString
setText x0 r = uncurryFields  r $ setText' x0
   where
    setText' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") String -> Tagged (SProxy "value") String -> Web3 e HexString
    setText' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SetTextFn)

--------------------------------------------------------------------------------
-- | ABIFn
--------------------------------------------------------------------------------


type ABIFn = Tagged (SProxy "ABI(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

aBI :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)), contentTypes :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) ByteString))
aBI x0 cm r = uncurryFields  r $ aBI' x0 cm
   where
    aBI' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "contentTypes") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) ByteString))
    aBI' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ABIFn)

--------------------------------------------------------------------------------
-- | SetPubkeyFn
--------------------------------------------------------------------------------


type SetPubkeyFn = Tagged (SProxy "setPubkey(bytes32,bytes32,bytes32)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)))

setPubkey :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), x :: (BytesN (D3 :& DOne D2)), y :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
setPubkey x0 r = uncurryFields  r $ setPubkey' x0
   where
    setPubkey' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "x") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "y") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    setPubkey' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SetPubkeyFn)

--------------------------------------------------------------------------------
-- | ContentFn
--------------------------------------------------------------------------------


type ContentFn = Tagged (SProxy "content(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

content :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
content x0 cm r = uncurryFields  r $ content' x0 cm
   where
    content' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    content' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: ContentFn)

--------------------------------------------------------------------------------
-- | AddrFn
--------------------------------------------------------------------------------


type AddrFn = Tagged (SProxy "addr(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

addr :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
addr x0 cm r = uncurryFields  r $ addr' x0 cm
   where
    addr' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    addr' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: AddrFn)

--------------------------------------------------------------------------------
-- | TextFn
--------------------------------------------------------------------------------


type TextFn = Tagged (SProxy "text(bytes32,string)") (Tuple2 (BytesN (D3 :& DOne D2)) String)

text :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)), key :: String } -> Web3 e (Either CallError String)
text x0 cm r = uncurryFields  r $ text' x0 cm
   where
    text' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") String -> Web3 e (Either CallError String)
    text' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: TextFn)

--------------------------------------------------------------------------------
-- | SetABIFn
--------------------------------------------------------------------------------


type SetABIFn = Tagged (SProxy "setABI(bytes32,uint256,bytes)") (Tuple3 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) ByteString)

setABI :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), contentType :: (UIntN (D2 :& D5 :& DOne D6)), data :: ByteString } -> Web3 e HexString
setABI x0 r = uncurryFields  r $ setABI' x0
   where
    setABI' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "contentType") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "data") ByteString -> Web3 e HexString
    setABI' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SetABIFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

name :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError String)
name x0 cm r = uncurryFields  r $ name' x0 cm
   where
    name' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError String)
    name' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: NameFn)

--------------------------------------------------------------------------------
-- | SetNameFn
--------------------------------------------------------------------------------


type SetNameFn = Tagged (SProxy "setName(bytes32,string)") (Tuple2 (BytesN (D3 :& DOne D2)) String)

setName :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), name :: String } -> Web3 e HexString
setName x0 r = uncurryFields  r $ setName' x0
   where
    setName' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "name") String -> Web3 e HexString
    setName' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetNameFn)

--------------------------------------------------------------------------------
-- | SetContentFn
--------------------------------------------------------------------------------


type SetContentFn = Tagged (SProxy "setContent(bytes32,bytes32)") (Tuple2 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)))

setContent :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), hash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
setContent x0 r = uncurryFields  r $ setContent' x0
   where
    setContent' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    setContent' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetContentFn)

--------------------------------------------------------------------------------
-- | PubkeyFn
--------------------------------------------------------------------------------


type PubkeyFn = Tagged (SProxy "pubkey(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

pubkey :: forall e. TransactionOptions NoPay -> ChainCursor -> { node :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2))))
pubkey x0 cm r = uncurryFields  r $ pubkey' x0 cm
   where
    pubkey' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2))))
    pubkey' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: PubkeyFn)

--------------------------------------------------------------------------------
-- | SetAddrFn
--------------------------------------------------------------------------------


type SetAddrFn = Tagged (SProxy "setAddr(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setAddr :: forall e. TransactionOptions NoPay -> { node :: (BytesN (D3 :& DOne D2)), addr :: Address } -> Web3 e HexString
setAddr x0 r = uncurryFields  r $ setAddr' x0
   where
    setAddr' :: TransactionOptions NoPay -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "addr") Address -> Web3 e HexString
    setAddr' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetAddrFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { ensAddr :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "ensAddr") Address -> Web3 e HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 (untagged y2 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | AddrChanged
--------------------------------------------------------------------------------


newtype AddrChanged = AddrChanged {node :: (BytesN (D3 :& DOne D2)),a :: Address}

derive instance newtypeAddrChanged :: Newtype AddrChanged _

instance eventFilterAddrChanged :: EventFilter AddrChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "52d7d861f09ab3d26239d492e8968629f95e9e318cf0b73bfddc441522a15fd2"),Nothing]

instance indexedEventAddrChanged :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "a") Address)) AddrChanged where
  isAnonymous _ = false

derive instance genericAddrChanged :: Generic AddrChanged _

instance eventGenericAddrChangedShow :: Show AddrChanged where
	show = genericShow

instance eventGenericAddrChangedeq :: Eq AddrChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ContentChanged
--------------------------------------------------------------------------------


newtype ContentChanged = ContentChanged {node :: (BytesN (D3 :& DOne D2)),hash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeContentChanged :: Newtype ContentChanged _

instance eventFilterContentChanged :: EventFilter ContentChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0424b6fe0d9c3bdbece0e7879dc241bb0c22e900be8b6c168b4ee08bd9bf83bc"),Nothing]

instance indexedEventContentChanged :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "hash") (BytesN (D3 :& DOne D2)))) ContentChanged where
  isAnonymous _ = false

derive instance genericContentChanged :: Generic ContentChanged _

instance eventGenericContentChangedShow :: Show ContentChanged where
	show = genericShow

instance eventGenericContentChangedeq :: Eq ContentChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | NameChanged
--------------------------------------------------------------------------------


newtype NameChanged = NameChanged {node :: (BytesN (D3 :& DOne D2)),name :: String}

derive instance newtypeNameChanged :: Newtype NameChanged _

instance eventFilterNameChanged :: EventFilter NameChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b7d29e911041e8d9b843369e890bcb72c9388692ba48b65ac54e7214c4c348f7"),Nothing]

instance indexedEventNameChanged :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "name") String)) NameChanged where
  isAnonymous _ = false

derive instance genericNameChanged :: Generic NameChanged _

instance eventGenericNameChangedShow :: Show NameChanged where
	show = genericShow

instance eventGenericNameChangedeq :: Eq NameChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ABIChanged
--------------------------------------------------------------------------------


newtype ABIChanged = ABIChanged {node :: (BytesN (D3 :& DOne D2)),contentType :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeABIChanged :: Newtype ABIChanged _

instance eventFilterABIChanged :: EventFilter ABIChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "aa121bbeef5f32f5961a2a28966e769023910fc9479059ee3495d4c1a696efe3"),Nothing,Nothing]

instance indexedEventABIChanged :: IndexedEvent (Tuple2 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "contentType") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) ABIChanged where
  isAnonymous _ = false

derive instance genericABIChanged :: Generic ABIChanged _

instance eventGenericABIChangedShow :: Show ABIChanged where
	show = genericShow

instance eventGenericABIChangedeq :: Eq ABIChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PubkeyChanged
--------------------------------------------------------------------------------


newtype PubkeyChanged = PubkeyChanged {node :: (BytesN (D3 :& DOne D2)),x :: (BytesN (D3 :& DOne D2)),y :: (BytesN (D3 :& DOne D2))}

derive instance newtypePubkeyChanged :: Newtype PubkeyChanged _

instance eventFilterPubkeyChanged :: EventFilter PubkeyChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1d6f5e03d3f63eb58751986629a5439baee5079ff04f345becb66e23eb154e46"),Nothing]

instance indexedEventPubkeyChanged :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple2 (Tagged (SProxy "x") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "y") (BytesN (D3 :& DOne D2)))) PubkeyChanged where
  isAnonymous _ = false

derive instance genericPubkeyChanged :: Generic PubkeyChanged _

instance eventGenericPubkeyChangedShow :: Show PubkeyChanged where
	show = genericShow

instance eventGenericPubkeyChangedeq :: Eq PubkeyChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | TextChanged
--------------------------------------------------------------------------------


newtype TextChanged = TextChanged {node :: (BytesN (D3 :& DOne D2)),indexedKey :: String,key :: String}

derive instance newtypeTextChanged :: Newtype TextChanged _

instance eventFilterTextChanged :: EventFilter TextChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d8c9334b1a9c2f9da342a0a2b32629c1a229b6445dad78947f674b44444a7550"),Nothing]

instance indexedEventTextChanged :: IndexedEvent (Tuple1 (Tagged (SProxy "node") (BytesN (D3 :& DOne D2)))) (Tuple2 (Tagged (SProxy "indexedKey") String) (Tagged (SProxy "key") String)) TextChanged where
  isAnonymous _ = false

derive instance genericTextChanged :: Generic TextChanged _

instance eventGenericTextChangedShow :: Show TextChanged where
	show = genericShow

instance eventGenericTextChangedeq :: Eq TextChanged where
	eq = genericEq