--------------------------------------------------------------------------------
-- | ReverseRegistrar
--------------------------------------------------------------------------------

module SecureVote.Contracts.ReverseRegistrar where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (Tuple1(..))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | ClaimFn
--------------------------------------------------------------------------------


type ClaimFn = Tagged (SProxy "claim(address)") (Tuple1 Address)

claim :: forall e. TransactionOptions NoPay -> { owner :: Address } -> Web3 e HexString
claim x0 r = uncurryFields  r $ claim' x0
   where
    claim' :: TransactionOptions NoPay -> Tagged (SProxy "owner") Address -> Web3 e HexString
    claim' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: ClaimFn)