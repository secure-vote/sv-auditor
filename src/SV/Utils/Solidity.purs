module SV.Utils.Solidity where

import SV.Prelude

import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Web3 (class KnownSize, D2, D5, D6, DCons, DOne, UIntN, unUIntN)

uint256ToIntUnsafe :: UIntN (DCons D2 (DCons D5 (DOne D6))) -> Int
uint256ToIntUnsafe = unsafeToInt <<< unUIntN


uintToIntUnsafe :: forall m a n. KnownSize n => UIntN n -> Int
uintToIntUnsafe = unsafeToInt <<< unUIntN
