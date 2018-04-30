--------------------------------------------------------------------------------
-- | Triggerable
--------------------------------------------------------------------------------

module SecureVote.Contracts.Triggerable where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, DOne, Tuple1(..))
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | HandleFn
--------------------------------------------------------------------------------


type HandleFn = Tagged (SProxy "handle(bytes32[])") (Tuple1 (Array (BytesN (D3 :& DOne D2))))

handle :: forall e. TransactionOptions NoPay -> { ref :: (Array (BytesN (D3 :& DOne D2))) } -> Web3 e HexString
handle x0 r = uncurryFields  r $ handle' x0
   where
    handle' :: TransactionOptions NoPay -> Tagged (SProxy "ref") (Array (BytesN (D3 :& DOne D2))) -> Web3 e HexString
    handle' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: HandleFn)