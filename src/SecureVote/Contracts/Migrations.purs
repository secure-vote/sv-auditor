--------------------------------------------------------------------------------
-- | Migrations
--------------------------------------------------------------------------------

module SecureVote.Contracts.Migrations where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | UpgradeFn
--------------------------------------------------------------------------------


type UpgradeFn = Tagged (SProxy "upgrade(address)") (Tuple1 Address)

upgrade :: forall e. TransactionOptions NoPay -> { new_address :: Address } -> Web3 e HexString
upgrade x0 r = uncurryFields  r $ upgrade' x0
   where
    upgrade' :: TransactionOptions NoPay -> Tagged (SProxy "new_address") Address -> Web3 e HexString
    upgrade' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeFn)

--------------------------------------------------------------------------------
-- | Last_completed_migrationFn
--------------------------------------------------------------------------------


type Last_completed_migrationFn = Tagged (SProxy "last_completed_migration()") (Tuple0 )

last_completed_migration :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
last_completed_migration x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: Last_completed_migrationFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SetCompletedFn
--------------------------------------------------------------------------------


type SetCompletedFn = Tagged (SProxy "setCompleted(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setCompleted :: forall e. TransactionOptions NoPay -> { completed :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setCompleted x0 r = uncurryFields  r $ setCompleted' x0
   where
    setCompleted' :: TransactionOptions NoPay -> Tagged (SProxy "completed") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setCompleted' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetCompletedFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)