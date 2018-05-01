module SV.Light.Counts.BinaryVote where

import SV.Light.Types.RunBallot
import SV.Prelude

import Data.Array as Arr
import Data.Foldable (foldr)
import Data.Map as Map
import Network.Ethereum.Web3 (embed, mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Utils.BigNumber (bnFromMDef0)


countBinary :: Array GetVoteResult -> Array BallotOptResult
countBinary weightedBallots =
    let ballotYes = unsafePartial fromJust $ mkHexString "8000000000000000000000000000000000000000000000000000000000000000"
        ballotNo = unsafePartial fromJust $ mkHexString "4000000000000000000000000000000000000000000000000000000000000000"

        resultsMap = foldr (\{ballot: {ballot}, bal} m ->
                            Map.alter (bnFromMDef0 >>> (+) bal >>> Just) ballot m)
                        Map.empty weightedBallots
        getVoteCount matchThis = bnFromMDef0 $ Map.lookup matchThis resultsMap
        countYes = getVoteCount ballotYes
        countNo = getVoteCount ballotNo
        invalidCount = foldr (\c sum -> c + sum) (embed 0) resultsMap - countYes - countNo

        nVotesMap = foldr (\{ballot: {ballot}, bal} m ->
                            Map.alter (fromMaybe 0 >>> (+) 1 >>> Just) ballot m)
                        Map.empty weightedBallots
        getNVotes matchThis = fromMaybe 0 $ Map.lookup matchThis nVotesMap
        nVotesYes = getNVotes ballotYes
        nVotesNo = getNVotes ballotNo
        invalidNVotes = Arr.length weightedBallots - nVotesYes - nVotesNo
    in
    [ {name: "yes", count: countYes, nVotes: nVotesYes}
    , {name: "no", count: countNo, nVotes: nVotesNo}
    , {name: "invalid ballots", count: invalidCount, nVotes: invalidNVotes}
    ]
