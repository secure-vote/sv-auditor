module SV.Utils.BigNumber where

import SV.Prelude

import Data.Decimal (Decimal)
import Data.Decimal (fromString) as Dec
import Data.Int (decimal)
import Network.Ethereum.Core.BigNumber as BN
import Network.Ethereum.Web3 (BigNumber, embed)
import Partial.Unsafe (unsafePartial)

bnFromMDef0 :: Maybe BigNumber -> BigNumber
bnFromMDef0 = fromMaybe (embed 0)

bnToStr :: BigNumber -> String
bnToStr = BN.toString decimal

bnToDec :: BigNumber -> Decimal
bnToDec = bnToStr >>> Dec.fromString >>> unsafePartial fromJust
