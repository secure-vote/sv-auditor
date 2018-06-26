module Test.SV.Main where

import Data.Maybe (Maybe(..))
import Test.SV.BallotSpec (ballotSpecTests)
-- -- import Test.SV.CompleteCycle (completeBallotTest)
-- import Test.SV.Encryption (encTests)
-- -- import Test.SV.EthTests (ethTests)
-- import Test.SV.HexBinTests (hexBinTests, intBitTests)
-- -- import Test.SV.SpecProperties (specProperties)
-- import Test.SV.SvLight.Counts.RangeVote (rangeVoteTests)
-- import Test.SV.SvLight.Delegation (delegationTests)
-- import Test.SV.SvLight.GetBallot (getBallotTests)
-- import Test.SV.UtilTests (utilTests)
import Test.Spec (describe)
-- import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run')


main = run' { slow: 120000, timeout: Just 180000 } [consoleReporter] do
  describe "Voting Auditor Tests" do
    describe "BallotSpec tests" ballotSpecTests
    -- describe "Hex / Binary tests" hexBinTests
    -- describe "BitString tests" intBitTests
    -- describe "Encryption Tests" encTests
    -- -- describe "Old Ballot Spec Tests" specProperties
    -- describe "Util tests" utilTests
    -- describe "Get new ballots" getBallotTests
    -- describe "Delegation Tests" delegationTests
    -- -- describe "Eth Tests" ethTests
    -- describe "Range Voting Tests" rangeVoteTests
    -- -- describe "A complete ballot test" completeBallotTest
