module Test.SV.BallotSpec where

import SV.Prelude

import Control.Monad.Aff.Class (liftAff)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress)
import Partial.Unsafe (unsafePartial)
import SV.Light.Types.Ballot (BallotSpec(..), OptsOuter(..), SimpleOption(..), SimpleVer(..))
import Simple.JSON (readJSON)
import Test.SV.Types (SpecType)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)


bSpecV1Ex1 = """{
    "ballotVersion": 1,
    "ballotInner": {
        "ballotTitle": "test1",
        "shortDesc": "sd",
        "longDesc": "ld",
        "startTime": 1234,
        "endTime": 5678,
        "erc20Addr": "0x9e88613418cF03dCa54D6a2cf6Ad934A78C7A17A",
        "discussionLink": null,
        "binding": true,
        "encryptionPK": null,
        "options": {
            "optionsVersion": 2,
            "options": null
        }
    }
}
"""


bSpecV2Ex1 = """{
  "ballotVersion": 2,
  "ballotInner": {
    "ballotTitle": "Title",
    "shortDesc": "short",
    "longDesc": "long",
    "subgroup": null,
    "discussionLink": null,
    "encryptionPK": null
  },
  "optionsVersion": 3,
  "optionsInner": {
    "options": null,
    "aux": null
  }
}"""


bSpecV2Ex2 = """{
  "ballotVersion": 2,
  "ballotInner": {
    "ballotTitle": "Title",
    "shortDesc": "short",
    "longDesc": "long",
    "subgroup": null,
    "discussionLink": "http://tokenvote.io/",
    "encryptionPK": null
  },
  "optionsVersion": 1,
  "optionsInner": {
    "options": [
        {"optionTitle": "o1", "optionDesc": "o1-desc"},
        {"optionTitle": "o2", "optionDesc": null}
    ],
    "aux": null
  }
}"""


ballotSpecTests :: forall e. SpecType e
ballotSpecTests = do
    it "should decode v2 - petition" $ do
        resEx1 <- eToAff $ readJSON bSpecV2Ex1
        resEx1 `shouldEqual` BVer02
            { ballotTitle: "Title"
            , shortDesc: "short"
            , longDesc: "long"
            , subgroup: Nothing
            , discussionLink: Nothing
            , encryptionPK: Nothing
            } OptsPetition

    it "should decode v2 - range" $ do
        resEx2 <- eToAff $ readJSON bSpecV2Ex2
        resEx2 `shouldEqual` BVer02
            { ballotTitle: "Title"
            , shortDesc: "short"
            , longDesc: "long"
            , subgroup: Nothing
            , discussionLink: Just "http://tokenvote.io/"
            , encryptionPK: Nothing
            } (OptsSimple RangeVotingPlusMinus3
                [ SimpleOption {optionTitle: "o1", optionDesc: Just "o1-desc"}
                , SimpleOption {optionTitle: "o2", optionDesc: Nothing}
                ])

    it "should decode v1 - binary" $ do
        resEx1 <- eToAff $ readJSON bSpecV1Ex1
        resEx1 `shouldEqual` BVer01
            { ballotTitle: "test1"
            , shortDesc: "sd"
            , longDesc: "ld"
            , startTime: 1234
            , endTime: 5678
            , erc20Addr: unsafePartial fromJust $ mkAddress =<< mkHexString "0x9e88613418cF03dCa54D6a2cf6Ad934A78C7A17A"
            , discussionLink: Nothing
            , binding: true
            , encryptionPK: Nothing
            , options: OptsBinary
            }
