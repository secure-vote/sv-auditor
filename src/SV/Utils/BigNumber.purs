module SV.Utils.BigNumber where

import SV.Prelude

import Data.Int (decimal)
import Network.Ethereum.Core.BigNumber as BN
import Network.Ethereum.Web3 (BigNumber, embed)

bnFromMDef0 :: Maybe BigNumber -> BigNumber
bnFromMDef0 = fromMaybe (embed 0)

bnToStr :: BigNumber -> String
bnToStr = BN.toString decimal
