--------------------------------------------------------------------------------
-- | EmitterTesting
--------------------------------------------------------------------------------

module SecureVote.Contracts.EmitterTesting where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (Tuple0, Tuple1(..), class IndexedEvent)
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | LogFn
--------------------------------------------------------------------------------


type LogFn = Tagged (SProxy "log(string)") (Tuple1 String)

log :: forall e. TransactionOptions NoPay -> { m :: String } -> Web3 e HexString
log x0 r = uncurryFields  r $ log' x0
   where
    log' :: TransactionOptions NoPay -> Tagged (SProxy "m") String -> Web3 e HexString
    log' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: LogFn)

--------------------------------------------------------------------------------
-- | Log
--------------------------------------------------------------------------------


newtype Log = Log {message :: String}

derive instance newtypeLog :: Newtype Log _

instance eventFilterLog :: EventFilter Log where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "cf34ef537ac33ee1ac626ca1587a0a7e8e51561e5514f8cb36afa1c5102b3bab")]

instance indexedEventLog :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "message") String)) Log where
  isAnonymous _ = false

derive instance genericLog :: Generic Log _

instance eventGenericLogShow :: Show Log where
	show = genericShow

instance eventGenericLogeq :: Eq Log where
	eq = genericEq