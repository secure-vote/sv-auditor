module SV.Light.Types.BallotBox where

-- | # Ballot Box Smart Contract Related Types

import SV.Prelude

import Control.Monad.Aff (Aff)
import Network.Ethereum.Web3 (Address, HexString)

-- | # Ballot Box Smart Contract Related Types

type BallotFromSC = {i :: Int, voterAddr :: Address, voterPk :: HexString, ballot :: HexString}


type BallotOperations =
    { getBallots :: (Int -> Aff () Unit -> Aff () (Array BallotFromSC))
    }


determineBallotVersion :: Address -> Aff _ (BallotOperations)
determineBallotVersion addr =
    -- todo: build out functions here for multi-ballot versions
    -- check if we're on a legacy ballot (swm-v1)
    -- check if we're using swm-v2 (first ballot spec options)
    -- check if we're using v3+
    pure { getBallots: (\i inc -> pure [])
         }
