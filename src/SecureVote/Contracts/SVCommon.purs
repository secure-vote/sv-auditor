--------------------------------------------------------------------------------
-- | SVCommon
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVCommon where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
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
-- | UpgradeAddrFn
--------------------------------------------------------------------------------


type UpgradeAddrFn = Tagged (SProxy "upgradeAddr()") (Tuple0 )

upgradeAddr :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
upgradeAddr x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: UpgradeAddrFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | UpgradeTimestampFn
--------------------------------------------------------------------------------


type UpgradeTimestampFn = Tagged (SProxy "upgradeTimestamp()") (Tuple0 )

upgradeTimestamp :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
upgradeTimestamp x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: UpgradeTimestampFn)

--------------------------------------------------------------------------------
-- | UpgradedFn
--------------------------------------------------------------------------------


type UpgradedFn = Tagged (SProxy "upgraded()") (Tuple0 )

upgraded :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
upgraded x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: UpgradedFn)

--------------------------------------------------------------------------------
-- | UndoUpgradeFn
--------------------------------------------------------------------------------


type UndoUpgradeFn = Tagged (SProxy "undoUpgrade()") (Tuple0 )

undoUpgrade :: forall e. TransactionOptions NoPay -> Web3 e HexString
undoUpgrade x0 = sendTx x0 ((tagged $ Tuple0 ) :: UndoUpgradeFn)

--------------------------------------------------------------------------------
-- | DeprecateAndUpgradeFn
--------------------------------------------------------------------------------


type DeprecateAndUpgradeFn = Tagged (SProxy "deprecateAndUpgrade(address)") (Tuple1 Address)

deprecateAndUpgrade :: forall e. TransactionOptions NoPay -> { _newSC :: Address } -> Web3 e HexString
deprecateAndUpgrade x0 r = uncurryFields  r $ deprecateAndUpgrade' x0
   where
    deprecateAndUpgrade' :: TransactionOptions NoPay -> Tagged (SProxy "_newSC") Address -> Web3 e HexString
    deprecateAndUpgrade' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DeprecateAndUpgradeFn)

--------------------------------------------------------------------------------
-- | ContractUpgraded
--------------------------------------------------------------------------------


newtype ContractUpgraded = ContractUpgraded {upgradeTime :: (UIntN (D2 :& D5 :& DOne D6)),newScAddr :: Address}

derive instance newtypeContractUpgraded :: Newtype ContractUpgraded _

instance eventFilterContractUpgraded :: EventFilter ContractUpgraded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "208a82eaf59643630be08b56d23aff3c045a061d5859a2d15e4e2cd7c31a1a43")]

instance indexedEventContractUpgraded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "upgradeTime") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "newScAddr") Address)) ContractUpgraded where
  isAnonymous _ = false

derive instance genericContractUpgraded :: Generic ContractUpgraded _

instance eventGenericContractUpgradedShow :: Show ContractUpgraded where
	show = genericShow

instance eventGenericContractUpgradedeq :: Eq ContractUpgraded where
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