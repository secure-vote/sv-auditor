module SV.Light.Types.Eth where

import Network.Ethereum.Web3 (type (:&), D2, D5, D6, DLProxy(..), DOne, UIntN)

type UInt256 = UIntN (D2 :& D5 :& DOne D6)

uint256Px :: DLProxy (D2 :& D5 :& (DOne D6))
uint256Px = DLProxy
