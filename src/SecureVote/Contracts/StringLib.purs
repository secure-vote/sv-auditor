--------------------------------------------------------------------------------
-- | StringLib
--------------------------------------------------------------------------------

module SecureVote.Contracts.StringLib where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | BytesToUIntFn
--------------------------------------------------------------------------------


type BytesToUIntFn = Tagged (SProxy "bytesToUInt(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

bytesToUInt :: forall e. TransactionOptions NoPay -> ChainCursor -> { v :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
bytesToUInt x0 cm r = uncurryFields  r $ bytesToUInt' x0 cm
   where
    bytesToUInt' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "v") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    bytesToUInt' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: BytesToUIntFn)

--------------------------------------------------------------------------------
-- | UintToBytesFn
--------------------------------------------------------------------------------


type UintToBytesFn = Tagged (SProxy "uintToBytes(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

uintToBytes :: forall e. TransactionOptions NoPay -> ChainCursor -> { v :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
uintToBytes x0 cm r = uncurryFields  r $ uintToBytes' x0 cm
   where
    uintToBytes' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "v") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    uintToBytes' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: UintToBytesFn)