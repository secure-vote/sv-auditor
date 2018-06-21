module SV.Utils.UInt where

import SV.Prelude

import Network.Ethereum.Web3 (class KnownSize, DLProxy(..), UIntN, embed, uIntNFromBigNumber)
import Partial.Unsafe (unsafePartial)

uintFromInt :: forall n. KnownSize n => Int -> UIntN n
uintFromInt i = unsafePartial fromJust $ uIntNFromBigNumber DLProxy $ embed i
