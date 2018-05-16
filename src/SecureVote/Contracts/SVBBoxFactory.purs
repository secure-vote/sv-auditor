--------------------------------------------------------------------------------
-- | SVBBoxFactory
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVBBoxFactory where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, Tuple4(..), UIntN)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | SpawnFn
--------------------------------------------------------------------------------


type SpawnFn = Tagged (SProxy "spawn(bytes32,uint256,address,address)") (Tuple4 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) Address Address)

spawn :: forall e. TransactionOptions NoPay -> { _specHash :: (BytesN (D3 :& DOne D2)), packed :: (UIntN (D2 :& D5 :& DOne D6)), ix :: Address, admin :: Address } -> Web3 e HexString
spawn x0 r = uncurryFields  r $ spawn' x0
   where
    spawn' :: TransactionOptions NoPay -> Tagged (SProxy "_specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "ix") Address -> Tagged (SProxy "admin") Address -> Web3 e HexString
    spawn' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: SpawnFn)