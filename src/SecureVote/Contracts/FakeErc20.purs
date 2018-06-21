--------------------------------------------------------------------------------
-- | FakeErc20
--------------------------------------------------------------------------------

module SecureVote.Contracts.FakeErc20 where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, deployContract)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | DecimalsFn
--------------------------------------------------------------------------------


type DecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

decimals :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (DOne D8)))
decimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DecimalsFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

balanceOf :: forall e. TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm r = uncurryFields  r $ balanceOf' x0 cm
   where
    balanceOf' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "_owner") Address -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    balanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | SymbolFn
--------------------------------------------------------------------------------


type SymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

symbol :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError String)
symbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SymbolFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)