--------------------------------------------------------------------------------
-- | SVBBoxFactory
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVBBoxFactory where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D6, D8, DOne, Tuple5(..), UIntN)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | SpawnFn
--------------------------------------------------------------------------------


type SpawnFn = Tagged (SProxy "spawn(bytes32,uint128,uint16,address,address)") (Tuple5 (BytesN (D3 :& DOne D2)) (UIntN (D1 :& D2 :& DOne D8)) (UIntN (D1 :& DOne D6)) Address Address)

spawn :: forall e. TransactionOptions NoPay -> { _specHash :: (BytesN (D3 :& DOne D2)), packedTimes :: (UIntN (D1 :& D2 :& DOne D8)), _submissionBits :: (UIntN (D1 :& DOne D6)), ix :: Address, admin :: Address } -> Web3 e HexString
spawn x0 r = uncurryFields  r $ spawn' x0
   where
    spawn' :: TransactionOptions NoPay -> Tagged (SProxy "_specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packedTimes") (UIntN (D1 :& D2 :& DOne D8)) -> Tagged (SProxy "_submissionBits") (UIntN (D1 :& DOne D6)) -> Tagged (SProxy "ix") Address -> Tagged (SProxy "admin") Address -> Web3 e HexString
    spawn' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: SpawnFn)