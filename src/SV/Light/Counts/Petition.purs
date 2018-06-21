module SV.Light.Counts.Petition where

import SV.Light.Types.RunBallot
import SV.Prelude

import Data.Array as Arr
import Data.Foldable (foldr, sum)
import Data.Map as Map
import Network.Ethereum.Web3 (embed, mkHexString)
import Partial.Unsafe (unsafePartial)
import SV.Utils.BigNumber (bnFromMDef0)


countPetition :: Array GetVoteResult -> Array BallotOptResult
countPetition weightedBallots =
    let revokeBallot = unsafePartial fromJust $ mkHexString "0000000000000000000000000000000000000000000000000000000000000000"
        signedPetition = unsafePartial fromJust $ mkHexString "0000000000000000000000000000000000000000000000000000000000000001"

        modBallot b = if b == revokeBallot then revokeBallot else signedPetition

        resultsMap = foldr (\{ballot: {ballot}, bal} m ->
                            Map.alter (bnFromMDef0 >>> (+) bal >>> Just) (modBallot ballot) m)
                        Map.empty weightedBallots
        getVoteCount matchThis = bnFromMDef0 $ Map.lookup matchThis resultsMap
        -- getVoteCountNot matchThis = bnFromMDef0 $ sum $ Map.filterKeys (\k -> k /= matchThis) resultsMap
        countYes = getVoteCount signedPetition
        countNo = getVoteCount revokeBallot
        invalidCount = foldr (\c sum -> c + sum) (embed 0) resultsMap - countYes - countNo

        nVotesMap = foldr (\{ballot: {ballot}, bal} m ->
                            Map.alter (fromMaybe 0 >>> (+) 1 >>> Just) ballot m)
                        Map.empty weightedBallots
        getNVotes matchThis = fromMaybe 0 $ Map.lookup matchThis nVotesMap
        nVotesYes = getNVotes signedPetition
        nVotesNo = getNVotes revokeBallot
        invalidNVotes = Arr.length weightedBallots - nVotesYes - nVotesNo
    in
    [ {name: "yes", count: countYes, nVotes: nVotesYes}
    , {name: "no", count: countNo, nVotes: nVotesNo}
    , {name: "invalid", count: invalidCount, nVotes: invalidNVotes}
    ]
