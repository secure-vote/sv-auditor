--------------------------------------------------------------------------------
-- | IxPaymentsSettingsIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxPaymentsSettingsIface where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple3(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei)
--------------------------------------------------------------------------------
-- | PayForDemocracyFn
--------------------------------------------------------------------------------


type PayForDemocracyFn = Tagged (SProxy "payForDemocracy(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

payForDemocracy :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
payForDemocracy x0 r = uncurryFields  r $ payForDemocracy' x0
   where
    payForDemocracy' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    payForDemocracy' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: PayForDemocracyFn)

--------------------------------------------------------------------------------
-- | GetBasicPricePerSecondFn
--------------------------------------------------------------------------------


type GetBasicPricePerSecondFn = Tagged (SProxy "getBasicPricePerSecond()") (Tuple0 )

getBasicPricePerSecond :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getBasicPricePerSecond x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBasicPricePerSecondFn)

--------------------------------------------------------------------------------
-- | GiveTimeToDemocFn
--------------------------------------------------------------------------------


type GiveTimeToDemocFn = Tagged (SProxy "giveTimeToDemoc(bytes32,uint256,bytes32)") (Tuple3 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)))

giveTimeToDemoc :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), additionalSeconds :: (UIntN (D2 :& D5 :& DOne D6)), ref :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
giveTimeToDemoc x0 r = uncurryFields  r $ giveTimeToDemoc' x0
   where
    giveTimeToDemoc' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "additionalSeconds") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "ref") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    giveTimeToDemoc' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: GiveTimeToDemocFn)

--------------------------------------------------------------------------------
-- | AccountInGoodStandingFn
--------------------------------------------------------------------------------


type AccountInGoodStandingFn = Tagged (SProxy "accountInGoodStanding(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

accountInGoodStanding :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
accountInGoodStanding x0 cm r = uncurryFields  r $ accountInGoodStanding' x0 cm
   where
    accountInGoodStanding' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    accountInGoodStanding' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: AccountInGoodStandingFn)

--------------------------------------------------------------------------------
-- | SetBasicPricePerSecondFn
--------------------------------------------------------------------------------


type SetBasicPricePerSecondFn = Tagged (SProxy "setBasicPricePerSecond(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setBasicPricePerSecond :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
setBasicPricePerSecond x0 r = uncurryFields  r $ setBasicPricePerSecond' x0
   where
    setBasicPricePerSecond' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    setBasicPricePerSecond' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetBasicPricePerSecondFn)

--------------------------------------------------------------------------------
-- | DowngradeToBasicFn
--------------------------------------------------------------------------------


type DowngradeToBasicFn = Tagged (SProxy "downgradeToBasic(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

downgradeToBasic :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
downgradeToBasic x0 r = uncurryFields  r $ downgradeToBasic' x0
   where
    downgradeToBasic' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    downgradeToBasic' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DowngradeToBasicFn)

--------------------------------------------------------------------------------
-- | SetPaymentEnabledFn
--------------------------------------------------------------------------------


type SetPaymentEnabledFn = Tagged (SProxy "setPaymentEnabled(bool)") (Tuple1 Boolean)

setPaymentEnabled :: forall e. TransactionOptions NoPay -> Boolean -> Web3 e HexString
setPaymentEnabled x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetPaymentEnabledFn)

--------------------------------------------------------------------------------
-- | PayoutAllFn
--------------------------------------------------------------------------------


type PayoutAllFn = Tagged (SProxy "payoutAll()") (Tuple0 )

payoutAll :: forall e. TransactionOptions NoPay -> Web3 e HexString
payoutAll x0 = sendTx x0 ((tagged $ Tuple0 ) :: PayoutAllFn)

--------------------------------------------------------------------------------
-- | UpgradeMeFn
--------------------------------------------------------------------------------


type UpgradeMeFn = Tagged (SProxy "upgradeMe(address)") (Tuple1 Address)

upgradeMe :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
upgradeMe x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: UpgradeMeFn)

--------------------------------------------------------------------------------
-- | SetPayToFn
--------------------------------------------------------------------------------


type SetPayToFn = Tagged (SProxy "setPayTo(address)") (Tuple1 Address)

setPayTo :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
setPayTo x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetPayToFn)

--------------------------------------------------------------------------------
-- | SetCommunityBallotFeeFn
--------------------------------------------------------------------------------


type SetCommunityBallotFeeFn = Tagged (SProxy "setCommunityBallotFee(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

setCommunityBallotFee :: forall e. TransactionOptions NoPay -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
setCommunityBallotFee x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetCommunityBallotFeeFn)

--------------------------------------------------------------------------------
-- | GetPremiumMultiplierFn
--------------------------------------------------------------------------------


type GetPremiumMultiplierFn = Tagged (SProxy "getPremiumMultiplier()") (Tuple0 )

getPremiumMultiplier :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (DOne D8)))
getPremiumMultiplier x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | GetPremiumPricePerSecondFn
--------------------------------------------------------------------------------


type GetPremiumPricePerSecondFn = Tagged (SProxy "getPremiumPricePerSecond()") (Tuple0 )

getPremiumPricePerSecond :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getPremiumPricePerSecond x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPremiumPricePerSecondFn)

--------------------------------------------------------------------------------
-- | GetPaymentEnabledFn
--------------------------------------------------------------------------------


type GetPaymentEnabledFn = Tagged (SProxy "getPaymentEnabled()") (Tuple0 )

getPaymentEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
getPaymentEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentEnabledFn)

--------------------------------------------------------------------------------
-- | GetCommunityBallotFeeFn
--------------------------------------------------------------------------------


type GetCommunityBallotFeeFn = Tagged (SProxy "getCommunityBallotFee()") (Tuple0 )

getCommunityBallotFee :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getCommunityBallotFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetCommunityBallotFeeFn)

--------------------------------------------------------------------------------
-- | UpgradeToPremiumFn
--------------------------------------------------------------------------------


type UpgradeToPremiumFn = Tagged (SProxy "upgradeToPremium(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

upgradeToPremium :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
upgradeToPremium x0 r = uncurryFields  r $ upgradeToPremium' x0
   where
    upgradeToPremium' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    upgradeToPremium' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeToPremiumFn)

--------------------------------------------------------------------------------
-- | SetPremiumMultiplierFn
--------------------------------------------------------------------------------


type SetPremiumMultiplierFn = Tagged (SProxy "setPremiumMultiplier(uint8)") (Tuple1 (UIntN (DOne D8)))

setPremiumMultiplier :: forall e. TransactionOptions NoPay -> { amount :: (UIntN (DOne D8)) } -> Web3 e HexString
setPremiumMultiplier x0 r = uncurryFields  r $ setPremiumMultiplier' x0
   where
    setPremiumMultiplier' :: TransactionOptions NoPay -> Tagged (SProxy "amount") (UIntN (DOne D8)) -> Web3 e HexString
    setPremiumMultiplier' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPremiumMultiplierFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getPayTo x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPayToFn)