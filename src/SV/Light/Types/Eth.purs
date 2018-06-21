module SV.Light.Types.Eth where

import Network.Ethereum.Web3 (type (:&), Address, BytesN, D2, D3, D4, D5, D6, DLProxy(..), DOne, UIntN)

type UInt256 = UIntN (D2 :& D5 :& DOne D6)

uint256Px :: DLProxy (D2 :& D5 :& (DOne D6))
uint256Px = DLProxy

type SCLocation = {address :: Address, network :: String}

type Bytes4 = BytesN (DOne D4)

bytes4Px :: DLProxy (DOne D4)
bytes4Px = DLProxy

bytes32Px :: DLProxy (D3 :& (DOne D2))
bytes32Px = DLProxy
