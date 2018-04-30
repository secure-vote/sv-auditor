--------------------------------------------------------------------------------
-- | IxCompleteIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxCompleteIface where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D6, D8, DOne, Tuple5(..), UIntN)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (HexString, TransactionOptions, Web3, Wei)
--------------------------------------------------------------------------------
-- | DeployBallotFn
--------------------------------------------------------------------------------


type DeployBallotFn = Tagged (SProxy "deployBallot(bytes32,bytes32,bytes32,uint128,uint16)") (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D1 :& D2 :& DOne D8)) (UIntN (D1 :& DOne D6)))

deployBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)), specHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), packedTimes :: (UIntN (D1 :& D2 :& DOne D8)), _submissionBits :: (UIntN (D1 :& DOne D6)) } -> Web3 e HexString
deployBallot x0 r = uncurryFields  r $ deployBallot' x0
   where
    deployBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packedTimes") (UIntN (D1 :& D2 :& DOne D8)) -> Tagged (SProxy "_submissionBits") (UIntN (D1 :& DOne D6)) -> Web3 e HexString
    deployBallot' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: DeployBallotFn)