module SV.Light.SmartContract where

import SV.Prelude

import Data.Lens ((^.))
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3.Solidity (Tuple3(..))
import SV.Light.Types.BallotBox (determineBallotVersion)
import SV.Light.Types.RunBallot (BallotBoxVersion(..), SmartContract, TxOpts)
import SecureVote.Contracts.BallotBoxVersion2 (specHash)
import SecureVote.Web3.Web3 (runWeb3_)


mkSC :: forall args e a. TxOpts -> SmartContract e args a
mkSC tos f c args = eToAff <=< eToAff <=< runWeb3_ $ f tos c args


determineBallotBoxVersion :: Address -> Aff _ BallotBoxVersion
determineBallotBoxVersion addr = do
    -- Tuple bVerM hasOldSpecHash <- sequential $ Tuple
    --         <$> runWeb3_
    hasOldSpecHash <- (isJust <<< join <<< eToM <<< map eToM) <$> runWeb3_ (specHash (defaultTransactionOptions # _to ^. addr) Latest)
    let hasOldSpecHash = false
        oldBVer = if hasOldSpecHash then BVer 2 else BVer 1
        bVerM = Nothing

    pure $ maybe oldBVer BBvN bVerM
