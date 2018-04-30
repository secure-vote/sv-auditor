--------------------------------------------------------------------------------
-- | SvEnsRegistrar
--------------------------------------------------------------------------------

module SecureVote.Contracts.SvEnsRegistrar where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, DOne, Tuple0(..), Tuple1(..), Tuple2(..), unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | RemAdminFn
--------------------------------------------------------------------------------


type RemAdminFn = Tagged (SProxy "remAdmin(address)") (Tuple1 Address)

remAdmin :: forall e. TransactionOptions NoPay -> { oldAdmin :: Address } -> Web3 e HexString
remAdmin x0 r = uncurryFields  r $ remAdmin' x0
   where
    remAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "oldAdmin") Address -> Web3 e HexString
    remAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RemAdminFn)

--------------------------------------------------------------------------------
-- | EnsFn
--------------------------------------------------------------------------------


type EnsFn = Tagged (SProxy "ens()") (Tuple0 )

ens :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
ens x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EnsFn)

--------------------------------------------------------------------------------
-- | ChOwnerFn
--------------------------------------------------------------------------------


type ChOwnerFn = Tagged (SProxy "chOwner(address,bool)") (Tuple2 Address Boolean)

chOwner :: forall e. TransactionOptions NoPay -> { newOwner :: Address, remPrevOwnerAsAdmin :: Boolean } -> Web3 e HexString
chOwner x0 r = uncurryFields  r $ chOwner' x0
   where
    chOwner' :: TransactionOptions NoPay -> Tagged (SProxy "newOwner") Address -> Tagged (SProxy "remPrevOwnerAsAdmin") Boolean -> Web3 e HexString
    chOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: ChOwnerFn)

--------------------------------------------------------------------------------
-- | AddAdminFn
--------------------------------------------------------------------------------


type AddAdminFn = Tagged (SProxy "addAdmin(address)") (Tuple1 Address)

addAdmin :: forall e. TransactionOptions NoPay -> { newAdmin :: Address } -> Web3 e HexString
addAdmin x0 r = uncurryFields  r $ addAdmin' x0
   where
    addAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    addAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: AddAdminFn)

--------------------------------------------------------------------------------
-- | RegisterNameFn
--------------------------------------------------------------------------------


type RegisterNameFn = Tagged (SProxy "registerName(string,address)") (Tuple2 String Address)

registerName :: forall e. TransactionOptions NoPay -> { subnodeStr :: String, _owner :: Address } -> Web3 e HexString
registerName x0 r = uncurryFields  r $ registerName' x0
   where
    registerName' :: TransactionOptions NoPay -> Tagged (SProxy "subnodeStr") String -> Tagged (SProxy "_owner") Address -> Web3 e HexString
    registerName' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: RegisterNameFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RegisterFn
--------------------------------------------------------------------------------


type RegisterFn = Tagged (SProxy "register(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

register :: forall e. TransactionOptions NoPay -> { subnode :: (BytesN (D3 :& DOne D2)), _owner :: Address } -> Web3 e HexString
register x0 r = uncurryFields  r $ register' x0
   where
    register' :: TransactionOptions NoPay -> Tagged (SProxy "subnode") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "_owner") Address -> Web3 e HexString
    register' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: RegisterFn)

--------------------------------------------------------------------------------
-- | RootNodeFn
--------------------------------------------------------------------------------


type RootNodeFn = Tagged (SProxy "rootNode()") (Tuple0 )

rootNode :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
rootNode x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: RootNodeFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,bytes32)") (Tuple2 Address (BytesN (D3 :& DOne D2)))

constructor :: forall e. TransactionOptions NoPay -> HexString -> { ensAddr :: Address, node :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "ensAddr") Address -> Tagged (SProxy "node") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ConstructorFn)