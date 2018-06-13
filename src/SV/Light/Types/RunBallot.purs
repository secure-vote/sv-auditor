module SV.Light.Types.RunBallot where

import SV.Prelude

import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Data.Map (Map)
import Network.Ethereum.Web3 (BytesN)
import Network.Ethereum.Web3.Types (Address, BigNumber, CallError, ChainCursor, ETH, NoPay, Web3(..))
import Network.Ethereum.Web3.Types (HexString, TransactionOptions(..))
import SV.Light.Types.Ballot (BallotSpec, OptsOuter)
import SV.Light.Types.BallotBox (BallotFromSC)


type BallotOptResult = {name :: String, count :: BigNumber, nVotes :: Int}
type BallotStrOptResult = {name :: String, count :: String, nVotes :: Int}
type BallotResult = {nVotes :: Int, ballotResults :: Array BallotStrOptResult}
type GetVoteResult = {origVoter :: Address, ballot :: BallotFromSC, bal :: BigNumber}

type TxOpts = TransactionOptions NoPay
type RunBallotArgs e = {bInfo :: BallotInfo e, bSpec :: BallotSpec, bbTos :: TxOpts, ercTos :: TxOpts, dlgtTos :: TxOpts, silent :: Boolean, dev :: Boolean}

type BallotScAddress = String

-- | Web3 Requester
type Web3Reqer m e a r = (TransactionOptions NoPay -> ChainCursor -> a -> Web3 e (Either CallError r)) -> ChainCursor -> a -> m e r


type SmartContract e args a = (TransactionOptions NoPay -> ChainCursor -> args -> Web3 (ref :: REF, console :: CONSOLE, avar :: AVAR | e) (Either CallError a)) -> ChainCursor -> args -> Aff (eth :: ETH, ref :: REF, console :: CONSOLE, avar :: AVAR | e) a


type BalanceMap = Map Address BigNumber
type BallotMap = Map Address BallotFromSC
type DelegateMap = Map Address Address


data BallotBoxVersion = BVer Int


type BallotOperations e =
    { getRawBallotsWDupes :: (Int -> Aff (| e) Unit -> Aff (| e) (Array BallotFromSC))
    , ballotOk :: Aff (| e) Boolean
    , getDelegateMap :: Aff (| e) DelegateMap
    , getBalanceMap :: Aff (| e) BalanceMap
    , bOptions :: OptsOuter
    , encPk :: Maybe HexString
    , getEncSeckey :: Aff (| e) HexString
    , getNVotes :: Aff (| e) Int
    }


type BallotInfo e =
    { bHash :: HexString
    , startTime :: Int
    , endTime :: Int
    , encSecKey :: Maybe HexString
    , nVotesCast :: Int
    , creationBlock :: Int
    , ballotOps :: BallotOperations e
    }


mkBallotInfo :: forall e. HexString -> Int -> Int -> Maybe HexString -> Int -> Int -> BallotOperations e -> BallotInfo e
mkBallotInfo = {bHash: _, startTime: _, endTime: _, encSecKey: _, nVotesCast: _, creationBlock: _, ballotOps: _}
