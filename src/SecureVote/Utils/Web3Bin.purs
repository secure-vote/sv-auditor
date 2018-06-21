module SecureVote.Utils.Web3Bin where

import SV.Prelude

import Network.Ethereum.Core.HexString (toByteString)
import Network.Ethereum.Web3 (BytesN, DLProxy(..), HexString, fromByteString, unBytesN)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize)
import SecureVote.Utils.Binary (bsToHexStr)

bytesNToHex :: forall n. KnownSize n => BytesN n -> HexString
bytesNToHex = bsToHexStr <<< unBytesN

hexToBytesN :: forall n. KnownSize n => HexString -> Maybe (BytesN n)
hexToBytesN hx = fromByteString DLProxy $ toByteString hx
