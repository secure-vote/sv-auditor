module SV.Light.SCs.BBFarmHelpers where

import SV.Prelude

import Network.Ethereum.Web3.Solidity (Tuple10(..))
import SecureVote.Contracts.BBFarm (getDetails)

specHash ballotId = \txOpts chainCursor {voter} -> map extract <$> getDetails txOpts chainCursor {voter, ballotId}
  where
    extract (Tuple10 _ _ _ _ _ _ sh _ _ _) = sh
