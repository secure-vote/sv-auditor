module SecureVote.Web3.Web3
    ( EthNetwork(..)
    , setNetwork
    , runWeb3_
    , runWeb3Dev
    , runWeb3Prod
    , runWeb3Classic
    , zeroHash
    , zeroAddr
    , setProvider
    ) where

import SV.Prelude

import Control.Monad.Aff (Milliseconds(..), delay)
import Control.Monad.Aff.AVar (putVar, takeVar)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Eff.AVar (AVar, makeVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Loops (whileM_)
import Control.Parallel (parallel, sequential)
import Data.Either (isLeft)
import Data.Foldable (oneOf)
import Network.Ethereum.Web3 (Address, ETH, Provider, Web3Error(..), httpProvider, mkAddress, mkHexString, runWeb3)
import Network.Ethereum.Web3.Types (Web3, HexString)
import Partial.Unsafe (unsafePartial)

upmhx :: String -> HexString
upmhx = unsafePartial fromJust <<< mkHexString

zeroHash :: HexString
zeroHash = upmhx "0x0000000000000000000000000000000000000000000000000000000000000000"
zeroAddr :: Address
zeroAddr = unsafePartial fromJust $ mkAddress $ upmhx "0x0000000000000000000000000000000000000000"


data EthNetwork
    = Mainnet
    | Kovan
    | Classic
    | Ropsten
    | OtherNet String


svMainnetProvider :: Provider
svMainnetProvider = unsafePerformEff $ httpProvider "https://mainnet.eth.secure.vote:8545"

svKovanProvider :: Provider
svKovanProvider = unsafePerformEff $ httpProvider "https://kovan.eth.secure.vote:8545"

svClassicProvider :: Provider
svClassicProvider = unsafePerformEff $ httpProvider "https://classic.eth.secure.vote:8545"

svRopstenProvider :: Provider
svRopstenProvider = unsafePerformEff $ httpProvider "https://ropsten.eth.secure.vote:8545"


_getNet :: EthNetwork -> Provider
_getNet n = case n of
    Mainnet -> svMainnetProvider
    Kovan -> svKovanProvider
    Classic -> svClassicProvider
    Ropsten -> svRopstenProvider
    OtherNet p -> unsafePerformEff $ httpProvider p


setNetwork :: forall e. EthNetwork -> Aff (ref :: REF | e) Unit
setNetwork n =  liftEff $ writeRef _svNetVar n


setProvider :: forall e. String -> Eff (ref :: REF | e) Unit
setProvider p = writeRef _svNetVar $ OtherNet p


_svNetVar :: Ref EthNetwork
_svNetVar = unsafePerformEff $ newRef Mainnet

-- | runWeb3 proxy via our mainnet provider
runWeb3Prod :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Prod = runWeb3 svMainnetProvider

runWeb3Dev :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Dev = runWeb3 svKovanProvider

runWeb3Classic :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Classic = runWeb3 svClassicProvider

runWeb3Ropsten :: forall e a. Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3Ropsten = runWeb3 svClassicProvider

-- | tracks concurrent web3 requests
concurrentWeb3Requests :: AVar Int
concurrentWeb3Requests =  unsafePerformEff $ makeVar 0

-- | will wait (with a delay of 50ms between checks) for a free slot to
-- | perform the web3 request
awaitWeb3Slot :: Aff _ Unit
awaitWeb3Slot = whileM_ shouldWait (delay $ Milliseconds 50.0)
  where
    shouldWait = do
        n <- takeVar concurrentWeb3Requests
        if n < 20
            then do
                putVar (n+1) concurrentWeb3Requests
                pure false
            else do
                putVar n concurrentWeb3Requests
                pure true
    maxRequests = 20

-- | decrement web3 request counts
web3SlotDone :: Aff _ Unit
web3SlotDone = do
    n <- takeVar concurrentWeb3Requests
    putVar (n-1) concurrentWeb3Requests

-- | Run a web3 request while both await a web3 slot (so we only run so many
-- | requests concurrently) and use the current global provider
runWeb3_ :: forall e eff a. Web3 _ a -> Aff _ (Either Web3Error a)
runWeb3_ w3r = do
    net <- liftEff $ readRef _svNetVar
    awaitWeb3Slot
    resp <- go net 0
    web3SlotDone
    pure resp
  where
    go net n = do
        res <- sequential $ oneOf
            [ parallel $ runWeb3 (_getNet net) w3r
            , parallel $ delay (Milliseconds $ timeoutSec * 1000.0)
                *> pure (Left $ RemoteError $ "Timeout reached (" <> show timeoutSec <> "s)")
            ]
        if isLeft res && n < tryTimes
            then do
                AffC.warn $ "Warning: Web3 request failed. Previous tries: "
                            <> show n <> ". Retrying..."
                go net (n+1)
            else pure res
    tryTimes = 3
    timeoutSec = 15.0
