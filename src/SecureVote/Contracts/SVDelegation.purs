--------------------------------------------------------------------------------
-- | SVDelegation
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVDelegation where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3, Tuple4, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | TotalDelegationsFn
--------------------------------------------------------------------------------


type TotalDelegationsFn = Tagged (SProxy "totalDelegations()") (Tuple0 )

totalDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalDelegations x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalDelegationsFn)

--------------------------------------------------------------------------------
-- | HistoricalDelegationsFn
--------------------------------------------------------------------------------


type HistoricalDelegationsFn = Tagged (SProxy "historicalDelegations(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

historicalDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
historicalDelegations x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: HistoricalDelegationsFn)

--------------------------------------------------------------------------------
-- | FnT_rawGetTokenDelegationFn
--------------------------------------------------------------------------------


type FnT_rawGetTokenDelegationFn = Tagged (SProxy "_rawGetTokenDelegation(address,address)") (Tuple2 Address Address)

_rawGetTokenDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { _voter :: Address, _tokenContract :: Address } -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
_rawGetTokenDelegation x0 cm r = uncurryFields  r $ _rawGetTokenDelegation' x0 cm
   where
    _rawGetTokenDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_voter") Address -> Tagged (SProxy "_tokenContract") Address -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
    _rawGetTokenDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: FnT_rawGetTokenDelegationFn)

--------------------------------------------------------------------------------
-- | ResolveDelegationFn
--------------------------------------------------------------------------------


type ResolveDelegationFn = Tagged (SProxy "resolveDelegation(address,address)") (Tuple2 Address Address)

resolveDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: Address, tokenContract :: Address } -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
resolveDelegation x0 cm r = uncurryFields  r $ resolveDelegation' x0 cm
   where
    resolveDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") Address -> Tagged (SProxy "tokenContract") Address -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
    resolveDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ResolveDelegationFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SetGlobalDelegationFn
--------------------------------------------------------------------------------


type SetGlobalDelegationFn = Tagged (SProxy "setGlobalDelegation(address)") (Tuple1 Address)

setGlobalDelegation :: forall e. TransactionOptions NoPay -> { dlgtAddress :: Address } -> Web3 e HexString
setGlobalDelegation x0 r = uncurryFields  r $ setGlobalDelegation' x0
   where
    setGlobalDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "dlgtAddress") Address -> Web3 e HexString
    setGlobalDelegation' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetGlobalDelegationFn)

--------------------------------------------------------------------------------
-- | SetTokenDelegationFn
--------------------------------------------------------------------------------


type SetTokenDelegationFn = Tagged (SProxy "setTokenDelegation(address,address)") (Tuple2 Address Address)

setTokenDelegation :: forall e. TransactionOptions NoPay -> { tokenContract :: Address, dlgtAddress :: Address } -> Web3 e HexString
setTokenDelegation x0 r = uncurryFields  r $ setTokenDelegation' x0
   where
    setTokenDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "tokenContract") Address -> Tagged (SProxy "dlgtAddress") Address -> Web3 e HexString
    setTokenDelegation' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetTokenDelegationFn)

--------------------------------------------------------------------------------
-- | FnT_rawGetGlobalDelegationFn
--------------------------------------------------------------------------------


type FnT_rawGetGlobalDelegationFn = Tagged (SProxy "_rawGetGlobalDelegation(address)") (Tuple1 Address)

_rawGetGlobalDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { _voter :: Address } -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
_rawGetGlobalDelegation x0 cm r = uncurryFields  r $ _rawGetGlobalDelegation' x0 cm
   where
    _rawGetGlobalDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_voter") Address -> Web3 e (Either CallError (Tuple4 (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
    _rawGetGlobalDelegation' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: FnT_rawGetGlobalDelegationFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | SetGlobalDelegation
--------------------------------------------------------------------------------


newtype SetGlobalDelegation = SetGlobalDelegation {voter :: Address,delegate :: Address}

derive instance newtypeSetGlobalDelegation :: Newtype SetGlobalDelegation _

instance eventFilterSetGlobalDelegation :: EventFilter SetGlobalDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "80e8ffc3c5dd5acf237f5c6e5855a312b8778e3df8ac7346f51155bcfeacf7cd")]

instance indexedEventSetGlobalDelegation :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "voter") Address) (Tagged (SProxy "delegate") Address)) SetGlobalDelegation where
  isAnonymous _ = false

derive instance genericSetGlobalDelegation :: Generic SetGlobalDelegation _

instance eventGenericSetGlobalDelegationShow :: Show SetGlobalDelegation where
	show = genericShow

instance eventGenericSetGlobalDelegationeq :: Eq SetGlobalDelegation where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetTokenDelegation
--------------------------------------------------------------------------------


newtype SetTokenDelegation = SetTokenDelegation {voter :: Address,tokenContract :: Address,delegate :: Address}

derive instance newtypeSetTokenDelegation :: Newtype SetTokenDelegation _

instance eventFilterSetTokenDelegation :: EventFilter SetTokenDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "74d96c2392d2b95d269942d650f623d0c7fb1f54a58e773709f4284f7b449cd7")]

instance indexedEventSetTokenDelegation :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "voter") Address) (Tagged (SProxy "tokenContract") Address) (Tagged (SProxy "delegate") Address)) SetTokenDelegation where
  isAnonymous _ = false

derive instance genericSetTokenDelegation :: Generic SetTokenDelegation _

instance eventGenericSetTokenDelegationShow :: Show SetTokenDelegation where
	show = genericShow

instance eventGenericSetTokenDelegationeq :: Eq SetTokenDelegation where
	eq = genericEq