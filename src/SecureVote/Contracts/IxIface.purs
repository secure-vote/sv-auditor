--------------------------------------------------------------------------------
-- | IxIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxIface where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple5(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei)
--------------------------------------------------------------------------------
-- | PayForDemocracyFn
--------------------------------------------------------------------------------


type PayForDemocracyFn = Tagged (SProxy "payForDemocracy(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

payForDemocracy :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
payForDemocracy x0 r = uncurryFields  r $ payForDemocracy' x0
   where
    payForDemocracy' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    payForDemocracy' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: PayForDemocracyFn)

--------------------------------------------------------------------------------
-- | AccountInGoodStandingFn
--------------------------------------------------------------------------------


type AccountInGoodStandingFn = Tagged (SProxy "accountInGoodStanding(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

accountInGoodStanding :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
accountInGoodStanding x0 cm r = uncurryFields  r $ accountInGoodStanding' x0 cm
   where
    accountInGoodStanding' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    accountInGoodStanding' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: AccountInGoodStandingFn)

--------------------------------------------------------------------------------
-- | DeployBallotFn
--------------------------------------------------------------------------------


type DeployBallotFn = Tagged (SProxy "deployBallot(bytes32,bytes32,bytes32,uint128,uint16)") (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D1 :& D2 :& DOne D8)) (UIntN (D1 :& DOne D6)))

deployBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)), specHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), packedTimes :: (UIntN (D1 :& D2 :& DOne D8)), _submissionBits :: (UIntN (D1 :& DOne D6)) } -> Web3 e HexString
deployBallot x0 r = uncurryFields  r $ deployBallot' x0
   where
    deployBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packedTimes") (UIntN (D1 :& D2 :& DOne D8)) -> Tagged (SProxy "_submissionBits") (UIntN (D1 :& DOne D6)) -> Web3 e HexString
    deployBallot' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: DeployBallotFn)

--------------------------------------------------------------------------------
-- | GetCommunityBallotFeeFn
--------------------------------------------------------------------------------


type GetCommunityBallotFeeFn = Tagged (SProxy "getCommunityBallotFee()") (Tuple0 )

getCommunityBallotFee :: forall e. TransactionOptions NoPay -> Web3 e HexString
getCommunityBallotFee x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetCommunityBallotFeeFn)

--------------------------------------------------------------------------------
-- | GetBallotAddrFn
--------------------------------------------------------------------------------


type GetBallotAddrFn = Tagged (SProxy "getBallotAddr(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getBallotAddr :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getBallotAddr x0 cm r = uncurryFields  r $ getBallotAddr' x0 cm
   where
    getBallotAddr' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getBallotAddr' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetBallotAddrFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> Web3 e HexString
getPayTo x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetPayToFn)