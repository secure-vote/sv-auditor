module SV.Light.SCs.ENS where

import SV.Prelude

import Network.Ethereum.Web3 (ChainCursor(..), mkHexString)
import SV.Light.SmartContracts (mkTos, runWeb3OrThrow)
import SecureVote.Contracts.ENSIface (resolver)
import SecureVote.Contracts.PublicResolver (addr)
import SecureVote.Utils.Monads (mToE)
import SecureVote.Utils.Web3Bin (bytesNToHex, hexToBytesN)
import SecureVote.Web3.Web3 (runWeb3_, zeroAddr)

foreign import namehashImpl :: String -> String

ensNameToNode ensName = mToE ("Calculating namehash for " <> ensName <> " failed") <<< (hexToBytesN <=< mkHexString) <<< namehashImpl $ ensName

lookupEns ensName ensLoc = do
    node <- eToAff $ ensNameToNode ensName
    lookupEnsNode node ensLoc

lookupEnsNode node ensLoc = do
    _resolver <- runWeb3OrThrow $ resolver (mkTos ensLoc.address) Latest {node}
    _addr <- runWeb3OrThrow $ addr (mkTos _resolver) Latest {node}
    pure _addr
