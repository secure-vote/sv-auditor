--------------------------------------------------------------------------------
-- | SVDelegationV0101Aux
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVDelegationV0101Aux where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | RevokePastDelegationsFn
--------------------------------------------------------------------------------


type RevokePastDelegationsFn = Tagged (SProxy "revokePastDelegations()") (Tuple0 )

revokePastDelegations :: forall e. TransactionOptions NoPay -> Web3 e HexString
revokePastDelegations x0 = sendTx x0 ((tagged $ Tuple0 ) :: RevokePastDelegationsFn)

--------------------------------------------------------------------------------
-- | DelegationsRevokedBeforeFn
--------------------------------------------------------------------------------


type DelegationsRevokedBeforeFn = Tagged (SProxy "delegationsRevokedBefore(address)") (Tuple1 Address)

delegationsRevokedBefore :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
delegationsRevokedBefore x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DelegationsRevokedBeforeFn)