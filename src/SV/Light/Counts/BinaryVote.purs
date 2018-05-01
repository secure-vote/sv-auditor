module SV.Light.Counts.BinaryVote where

import SV.Light.Types.RunBallot
import SV.Prelude

import Data.Foldable (foldr)
import Data.Map as Map
import Network.Ethereum.Web3 (embed, mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Utils.BigNumber (bnFromMDef0)


countBinary :: Array GetVoteResult -> Array BallotOptResult
countBinary weightedBallots =
    let resultsMap = foldr (\{ballot: {ballot}, bal} m ->
                            Map.alter (bnFromMDef0 >>> (+) bal >>> Just) ballot m)
                        Map.empty weightedBallots
        ballotYes = unsafePartial fromJust $ mkHexString "8000000000000000000000000000000000000000000000000000000000000000"
        ballotNo = unsafePartial fromJust $ mkHexString "4000000000000000000000000000000000000000000000000000000000000000"
        getNVotes matchThis = bnFromMDef0 $ Map.lookup matchThis resultsMap
        countYes = getNVotes ballotYes
        countNo = getNVotes ballotNo
    in
    [ {name: "yes", count: countYes}
    , {name: "no", count: countNo}
    , {name: "invalid ballots", count: embed (Map.size resultsMap) - countYes - countNo }
    ]
