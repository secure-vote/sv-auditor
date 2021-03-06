module SV.Light.Types.RunBallot where

import SV.Prelude

import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Data.Map (Map)
import Network.Ethereum.Web3.Types (Address, BigNumber, CallError, ChainCursor, ETH, NoPay, Web3(..))
import Network.Ethereum.Web3.Types (HexString, TransactionOptions(..))
import SV.Light.Types.Ballot (BallotSpec)
import SV.Light.Types.BallotBox (BallotFromSC, BallotOperations)
import SV.Light.Types.Eth (UInt256)


type BallotOptResult = {name :: String, count :: BigNumber, nVotes :: Int}
type BallotStrOptResult = {name :: String, count :: String, nVotes :: Int}
type BallotResult = {nVotes :: Int, ballotResults :: Array BallotStrOptResult}
type GetVoteResult = {origVoter :: Address, ballot :: BallotFromSC, bal :: BigNumber}

type TxOpts = TransactionOptions NoPay
type RunBallotArgs = {bInfo :: BallotInfo, bSpec :: BallotSpec, bbFarmTos :: TxOpts, ercTos :: TxOpts, dlgtTos :: TxOpts, silent :: Boolean, netId :: String, dev :: Boolean}


type BallotInfo =
    { ballotId :: UInt256
    , bHash :: HexString
    , startTime :: Int
    , endTime :: Int
    , encSecKey :: Maybe HexString
    , nVotesCast :: Int
    }


mkBallotInfo :: UInt256 -> HexString -> Int -> Int -> Maybe HexString -> Int -> BallotInfo
mkBallotInfo = {ballotId: _, bHash: _, startTime: _, endTime: _, encSecKey: _, nVotesCast: _}


type BallotScAddress = String


type Web3Reqer m e a r = (TransactionOptions NoPay -> ChainCursor -> a -> Web3 e (Either CallError r)) -> ChainCursor -> a -> m e r

type Web3Effs e = (ref :: REF, avar :: AVAR, console :: CONSOLE | e)
type SmartContract e args a = (TransactionOptions NoPay -> ChainCursor -> args -> Web3 (Web3Effs e) (Either CallError a)) -> ChainCursor -> args -> Aff (eth :: ETH | Web3Effs e) a


type BalanceMap = Map Address BigNumber
type BallotMap = Map Address BallotFromSC
type DelegateMap = Map Address Address
