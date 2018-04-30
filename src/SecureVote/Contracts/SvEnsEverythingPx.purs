--------------------------------------------------------------------------------
-- | SvEnsEverythingPx
--------------------------------------------------------------------------------

module SecureVote.Contracts.SvEnsEverythingPx where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | ResolverFn
--------------------------------------------------------------------------------


type ResolverFn = Tagged (SProxy "resolver()") (Tuple0 )

resolver :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
resolver x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ResolverFn)

--------------------------------------------------------------------------------
-- | RemAdminFn
--------------------------------------------------------------------------------


type RemAdminFn = Tagged (SProxy "remAdmin(address)") (Tuple1 Address)

remAdmin :: forall e. TransactionOptions NoPay -> { a :: Address } -> Web3 e HexString
remAdmin x0 r = uncurryFields  r $ remAdmin' x0
   where
    remAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "a") Address -> Web3 e HexString
    remAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RemAdminFn)

--------------------------------------------------------------------------------
-- | AdminLogFn
--------------------------------------------------------------------------------


type AdminLogFn = Tagged (SProxy "adminLog(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

adminLog :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
adminLog x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AdminLogFn)

--------------------------------------------------------------------------------
-- | RegistrarFn
--------------------------------------------------------------------------------


type RegistrarFn = Tagged (SProxy "registrar()") (Tuple0 )

registrar :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
registrar x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: RegistrarFn)

--------------------------------------------------------------------------------
-- | AdminsFn
--------------------------------------------------------------------------------


type AdminsFn = Tagged (SProxy "admins(address)") (Tuple1 Address)

admins :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
admins x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AdminsFn)

--------------------------------------------------------------------------------
-- | AddAdminFn
--------------------------------------------------------------------------------


type AddAdminFn = Tagged (SProxy "addAdmin(address)") (Tuple1 Address)

addAdmin :: forall e. TransactionOptions NoPay -> { a :: Address } -> Web3 e HexString
addAdmin x0 r = uncurryFields  r $ addAdmin' x0
   where
    addAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "a") Address -> Web3 e HexString
    addAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: AddAdminFn)

--------------------------------------------------------------------------------
-- | RegistryFn
--------------------------------------------------------------------------------


type RegistryFn = Tagged (SProxy "registry()") (Tuple0 )

registry :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
registry x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: RegistryFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RegNameFn
--------------------------------------------------------------------------------


type RegNameFn = Tagged (SProxy "regName(string,address)") (Tuple2 String Address)

regName :: forall e. TransactionOptions NoPay -> { name :: String, resolveTo :: Address } -> Web3 e HexString
regName x0 r = uncurryFields  r $ regName' x0
   where
    regName' :: TransactionOptions NoPay -> Tagged (SProxy "name") String -> Tagged (SProxy "resolveTo") Address -> Web3 e HexString
    regName' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: RegNameFn)

--------------------------------------------------------------------------------
-- | RegNameWOwnerFn
--------------------------------------------------------------------------------


type RegNameWOwnerFn = Tagged (SProxy "regNameWOwner(string,address,address)") (Tuple3 String Address Address)

regNameWOwner :: forall e. TransactionOptions NoPay -> { name :: String, resolveTo :: Address, domainOwner :: Address } -> Web3 e HexString
regNameWOwner x0 r = uncurryFields  r $ regNameWOwner' x0
   where
    regNameWOwner' :: TransactionOptions NoPay -> Tagged (SProxy "name") String -> Tagged (SProxy "resolveTo") Address -> Tagged (SProxy "domainOwner") Address -> Web3 e HexString
    regNameWOwner' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: RegNameWOwnerFn)

--------------------------------------------------------------------------------
-- | RootNodeFn
--------------------------------------------------------------------------------


type RootNodeFn = Tagged (SProxy "rootNode()") (Tuple0 )

rootNode :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
rootNode x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: RootNodeFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address,address,bytes32)") (Tuple4 Address Address Address (BytesN (D3 :& DOne D2)))

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _registrar :: Address, _registry :: Address, _resolver :: Address, _rootNode :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_registrar") Address -> Tagged (SProxy "_registry") Address -> Tagged (SProxy "_resolver") Address -> Tagged (SProxy "_rootNode") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 y5 = deployContract y0 bc' ((tagged $ Tuple4 (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 )) :: ConstructorFn)