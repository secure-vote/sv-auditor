--------------------------------------------------------------------------------
-- | SVAdminPxFactory
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVAdminPxFactory where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, DOne, Tuple3(..))
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | SpawnFn
--------------------------------------------------------------------------------


type SpawnFn = Tagged (SProxy "spawn(bytes32,address,address)") (Tuple3 (BytesN (D3 :& DOne D2)) Address Address)

spawn :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), initAdmin :: Address, fwdTo :: Address } -> Web3 e HexString
spawn x0 r = uncurryFields  r $ spawn' x0
   where
    spawn' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "initAdmin") Address -> Tagged (SProxy "fwdTo") Address -> Web3 e HexString
    spawn' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SpawnFn)