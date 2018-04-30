--------------------------------------------------------------------------------
-- | SVDelegationV0102
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVDelegationV0102 where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3, Tuple4, Tuple6, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | BackendFn
--------------------------------------------------------------------------------


type BackendFn = Tagged (SProxy "backend()") (Tuple0 )

backend :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
backend x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BackendFn)

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
-- | DoUpgradeFn
--------------------------------------------------------------------------------


type DoUpgradeFn = Tagged (SProxy "doUpgrade(address)") (Tuple1 Address)

doUpgrade :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
doUpgrade x0 r = uncurryFields  r $ doUpgrade' x0
   where
    doUpgrade' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    doUpgrade' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DoUpgradeFn)

--------------------------------------------------------------------------------
-- | ResolveDelegationFn
--------------------------------------------------------------------------------


type ResolveDelegationFn = Tagged (SProxy "resolveDelegation(address,address)") (Tuple2 Address Address)

resolveDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: Address, tokenAddress :: Address } -> Web3 e (Either CallError (Tuple6 (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) Address Address Address))
resolveDelegation x0 cm r = uncurryFields  r $ resolveDelegation' x0 cm
   where
    resolveDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") Address -> Tagged (SProxy "tokenAddress") Address -> Web3 e (Either CallError (Tuple6 (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) Address Address Address))
    resolveDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ResolveDelegationFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | GetUpgradePointerFn
--------------------------------------------------------------------------------


type GetUpgradePointerFn = Tagged (SProxy "getUpgradePointer()") (Tuple0 )

getUpgradePointer :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getUpgradePointer x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetUpgradePointerFn)

--------------------------------------------------------------------------------
-- | SetGlobalDelegationFn
--------------------------------------------------------------------------------


type SetGlobalDelegationFn = Tagged (SProxy "setGlobalDelegation(address)") (Tuple1 Address)

setGlobalDelegation :: forall e. TransactionOptions NoPay -> { delegate :: Address } -> Web3 e HexString
setGlobalDelegation x0 r = uncurryFields  r $ setGlobalDelegation' x0
   where
    setGlobalDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "delegate") Address -> Web3 e HexString
    setGlobalDelegation' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetGlobalDelegationFn)

--------------------------------------------------------------------------------
-- | GetDelegationIDFn
--------------------------------------------------------------------------------


type GetDelegationIDFn = Tagged (SProxy "getDelegationID(address,address)") (Tuple2 Address Address)

getDelegationID :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: Address, tokenAddress :: Address } -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
getDelegationID x0 cm r = uncurryFields  r $ getDelegationID' x0 cm
   where
    getDelegationID' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") Address -> Tagged (SProxy "tokenAddress") Address -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
    getDelegationID' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDelegationIDFn)

--------------------------------------------------------------------------------
-- | SetTokenDelegationFn
--------------------------------------------------------------------------------


type SetTokenDelegationFn = Tagged (SProxy "setTokenDelegation(address,address)") (Tuple2 Address Address)

setTokenDelegation :: forall e. TransactionOptions NoPay -> { delegate :: Address, tokenAddress :: Address } -> Web3 e HexString
setTokenDelegation x0 r = uncurryFields  r $ setTokenDelegation' x0
   where
    setTokenDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "delegate") Address -> Tagged (SProxy "tokenAddress") Address -> Web3 e HexString
    setTokenDelegation' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetTokenDelegationFn)

--------------------------------------------------------------------------------
-- | ResolveRawDelegationFn
--------------------------------------------------------------------------------


type ResolveRawDelegationFn = Tagged (SProxy "resolveRawDelegation(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

resolveRawDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: (BytesN (D3 :& DOne D2)), namespace :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
resolveRawDelegation x0 cm r = uncurryFields  r $ resolveRawDelegation' x0 cm
   where
    resolveRawDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "namespace") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
    resolveRawDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ResolveRawDelegationFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _backend :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_backend") Address -> Web3 e HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 (untagged y2 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | SetEthGlobalDelegation
--------------------------------------------------------------------------------


newtype SetEthGlobalDelegation = SetEthGlobalDelegation {voter :: Address,delegate :: Address}

derive instance newtypeSetEthGlobalDelegation :: Newtype SetEthGlobalDelegation _

instance eventFilterSetEthGlobalDelegation :: EventFilter SetEthGlobalDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "288a62efad381a1e5826af48b6285f4fdae4ce473689b753fc9326fa3b3cfddc")]

instance indexedEventSetEthGlobalDelegation :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "voter") Address) (Tagged (SProxy "delegate") Address)) SetEthGlobalDelegation where
  isAnonymous _ = false

derive instance genericSetEthGlobalDelegation :: Generic SetEthGlobalDelegation _

instance eventGenericSetEthGlobalDelegationShow :: Show SetEthGlobalDelegation where
	show = genericShow

instance eventGenericSetEthGlobalDelegationeq :: Eq SetEthGlobalDelegation where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetEthTokenDelegation
--------------------------------------------------------------------------------


newtype SetEthTokenDelegation = SetEthTokenDelegation {voter :: Address,delegate :: Address,tokenContract :: Address}

derive instance newtypeSetEthTokenDelegation :: Newtype SetEthTokenDelegation _

instance eventFilterSetEthTokenDelegation :: EventFilter SetEthTokenDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "96a20eba7f8c9a5091a2f37b6c192f78d284e2ad88db2fac40bc7639b08764a4")]

instance indexedEventSetEthTokenDelegation :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "voter") Address) (Tagged (SProxy "delegate") Address) (Tagged (SProxy "tokenContract") Address)) SetEthTokenDelegation where
  isAnonymous _ = false

derive instance genericSetEthTokenDelegation :: Generic SetEthTokenDelegation _

instance eventGenericSetEthTokenDelegationShow :: Show SetEthTokenDelegation where
	show = genericShow

instance eventGenericSetEthTokenDelegationeq :: Eq SetEthTokenDelegation where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetDelegation
--------------------------------------------------------------------------------


newtype SetDelegation = SetDelegation {voter :: (BytesN (D3 :& DOne D2)),delegate :: (BytesN (D3 :& DOne D2)),namespace :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetDelegation :: Newtype SetDelegation _

instance eventFilterSetDelegation :: EventFilter SetDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "62a82f5a6c625c49e6ec42e4c4e2071ac33a3183b718b39ec58d3559f83673ad")]

instance indexedEventSetDelegation :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "voter") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "delegate") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "namespace") (UIntN (D2 :& D5 :& DOne D6)))) SetDelegation where
  isAnonymous _ = false

derive instance genericSetDelegation :: Generic SetDelegation _

instance eventGenericSetDelegationShow :: Show SetDelegation where
	show = genericShow

instance eventGenericSetDelegationeq :: Eq SetDelegation where
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