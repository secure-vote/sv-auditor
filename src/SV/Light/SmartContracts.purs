module SV.Light.SmartContracts where

import SV.Prelude

import Data.Lens ((.~))
import Network.Ethereum.Web3 (_to, defaultTransactionOptions)
import SV.Light.Types.RunBallot (SmartContract, TxOpts)
import SecureVote.Web3.Web3 (runWeb3_)


mkSC :: forall args e a. TxOpts -> SmartContract e args a
mkSC tos f c args = eToAff <=< eToAff <=< runWeb3_ $ f tos c args


mkTos addr = defaultTransactionOptions # _to .~ Just addr


runWeb3OrThrow = eToAff <=< eToAff <=< runWeb3_
