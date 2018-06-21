--------------------------------------------------------------------------------
-- | SVIndex
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVIndex where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D1, D2, D3, D4, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | DDeprecateCategoryFn
--------------------------------------------------------------------------------


type DDeprecateCategoryFn = Tagged (SProxy "dDeprecateCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

dDeprecateCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), catId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dDeprecateCategory x0 r = uncurryFields  r $ dDeprecateCategory' x0
   where
    dDeprecateCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "catId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    dDeprecateCategory' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DDeprecateCategoryFn)

--------------------------------------------------------------------------------
-- | GetVersionFn
--------------------------------------------------------------------------------


type GetVersionFn = Tagged (SProxy "getVersion()") (Tuple0 )

getVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetVersionFn)

--------------------------------------------------------------------------------
-- | GetBBFarmFn
--------------------------------------------------------------------------------


type GetBBFarmFn = Tagged (SProxy "getBBFarm(uint8)") (Tuple1 (UIntN (DOne D8)))

getBBFarm :: forall e. TransactionOptions NoPay -> ChainCursor -> { bbFarmId :: (UIntN (DOne D8)) } -> Web3 e (Either CallError Address)
getBBFarm x0 cm r = uncurryFields  r $ getBBFarm' x0 cm
   where
    getBBFarm' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "bbFarmId") (UIntN (DOne D8)) -> Web3 e (Either CallError Address)
    getBBFarm' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetBBFarmFn)

--------------------------------------------------------------------------------
-- | SetOwnerFn
--------------------------------------------------------------------------------


type SetOwnerFn = Tagged (SProxy "setOwner(address)") (Tuple1 Address)

setOwner :: forall e. TransactionOptions NoPay -> { newOwner :: Address } -> Web3 e HexString
setOwner x0 r = uncurryFields  r $ setOwner' x0
   where
    setOwner' :: TransactionOptions NoPay -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetOwnerFn)

--------------------------------------------------------------------------------
-- | ReclaimTokenFn
--------------------------------------------------------------------------------


type ReclaimTokenFn = Tagged (SProxy "reclaimToken(address)") (Tuple1 Address)

reclaimToken :: forall e. TransactionOptions NoPay -> { token :: Address } -> Web3 e HexString
reclaimToken x0 r = uncurryFields  r $ reclaimToken' x0
   where
    reclaimToken' :: TransactionOptions NoPay -> Tagged (SProxy "token") Address -> Web3 e HexString
    reclaimToken' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: ReclaimTokenFn)

--------------------------------------------------------------------------------
-- | DeprecateBBFarmFn
--------------------------------------------------------------------------------


type DeprecateBBFarmFn = Tagged (SProxy "deprecateBBFarm(uint8,address)") (Tuple2 (UIntN (DOne D8)) Address)

deprecateBBFarm :: forall e. TransactionOptions NoPay -> { bbFarmId :: (UIntN (DOne D8)), _bbFarm :: Address } -> Web3 e HexString
deprecateBBFarm x0 r = uncurryFields  r $ deprecateBBFarm' x0
   where
    deprecateBBFarm' :: TransactionOptions NoPay -> Tagged (SProxy "bbFarmId") (UIntN (DOne D8)) -> Tagged (SProxy "_bbFarm") Address -> Web3 e HexString
    deprecateBBFarm' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DeprecateBBFarmFn)

--------------------------------------------------------------------------------
-- | DSetCommunityBallotsEnabledFn
--------------------------------------------------------------------------------


type DSetCommunityBallotsEnabledFn = Tagged (SProxy "dSetCommunityBallotsEnabled(bytes32,bool)") (Tuple2 (BytesN (D3 :& DOne D2)) Boolean)

dSetCommunityBallotsEnabled :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), enabled :: Boolean } -> Web3 e HexString
dSetCommunityBallotsEnabled x0 r = uncurryFields  r $ dSetCommunityBallotsEnabled' x0
   where
    dSetCommunityBallotsEnabled' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "enabled") Boolean -> Web3 e HexString
    dSetCommunityBallotsEnabled' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DSetCommunityBallotsEnabledFn)

--------------------------------------------------------------------------------
-- | DAddCategoryFn
--------------------------------------------------------------------------------


type DAddCategoryFn = Tagged (SProxy "dAddCategory(bytes32,bytes32,bool,uint256)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6)))

dAddCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), catName :: (BytesN (D3 :& DOne D2)), hasParent :: Boolean, parent :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dAddCategory x0 r = uncurryFields  r $ dAddCategory' x0
   where
    dAddCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "catName") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hasParent") Boolean -> Tagged (SProxy "parent") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    dAddCategory' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: DAddCategoryFn)

--------------------------------------------------------------------------------
-- | DDeployBallotFn
--------------------------------------------------------------------------------


type DDeployBallotFn = Tagged (SProxy "dDeployBallot(bytes32,bytes32,bytes32,uint256)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

dDeployBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)), specHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), packed :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dDeployBallot x0 r = uncurryFields  r $ dDeployBallot' x0
   where
    dDeployBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    dDeployBallot' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: DDeployBallotFn)

--------------------------------------------------------------------------------
-- | DoUpgradeFn
--------------------------------------------------------------------------------


type DoUpgradeFn = Tagged (SProxy "doUpgrade(address)") (Tuple1 Address)

doUpgrade :: forall e. TransactionOptions NoPay -> { nextSC :: Address } -> Web3 e HexString
doUpgrade x0 r = uncurryFields  r $ doUpgrade' x0
   where
    doUpgrade' :: TransactionOptions NoPay -> Tagged (SProxy "nextSC") Address -> Web3 e HexString
    doUpgrade' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DoUpgradeFn)

--------------------------------------------------------------------------------
-- | SetDNoEditorsFn
--------------------------------------------------------------------------------


type SetDNoEditorsFn = Tagged (SProxy "setDNoEditors(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

setDNoEditors :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
setDNoEditors x0 r = uncurryFields  r $ setDNoEditors' x0
   where
    setDNoEditors' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    setDNoEditors' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetDNoEditorsFn)

--------------------------------------------------------------------------------
-- | PayoutAllFn
--------------------------------------------------------------------------------


type PayoutAllFn = Tagged (SProxy "payoutAll()") (Tuple0 )

payoutAll :: forall e. TransactionOptions NoPay -> Web3 e HexString
payoutAll x0 = sendTx x0 ((tagged $ Tuple0 ) :: PayoutAllFn)

--------------------------------------------------------------------------------
-- | AddBBFarmFn
--------------------------------------------------------------------------------


type AddBBFarmFn = Tagged (SProxy "addBBFarm(address)") (Tuple1 Address)

addBBFarm :: forall e. TransactionOptions NoPay -> { bbFarm :: Address } -> Web3 e HexString
addBBFarm x0 r = uncurryFields  r $ addBBFarm' x0
   where
    addBBFarm' :: TransactionOptions NoPay -> Tagged (SProxy "bbFarm") Address -> Web3 e HexString
    addBBFarm' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: AddBBFarmFn)

--------------------------------------------------------------------------------
-- | GetBackendFn
--------------------------------------------------------------------------------


type GetBackendFn = Tagged (SProxy "getBackend()") (Tuple0 )

getBackend :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getBackend x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetBackendFn)

--------------------------------------------------------------------------------
-- | EnsOwnerPxFn
--------------------------------------------------------------------------------


type EnsOwnerPxFn = Tagged (SProxy "ensOwnerPx()") (Tuple0 )

ensOwnerPx :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
ensOwnerPx x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EnsOwnerPxFn)

--------------------------------------------------------------------------------
-- | DDisableErc20OwnerClaimFn
--------------------------------------------------------------------------------


type DDisableErc20OwnerClaimFn = Tagged (SProxy "dDisableErc20OwnerClaim(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

dDisableErc20OwnerClaim :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
dDisableErc20OwnerClaim x0 r = uncurryFields  r $ dDisableErc20OwnerClaim' x0
   where
    dDisableErc20OwnerClaim' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    dDisableErc20OwnerClaim' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DDisableErc20OwnerClaimFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | DDowngradeToBasicFn
--------------------------------------------------------------------------------


type DDowngradeToBasicFn = Tagged (SProxy "dDowngradeToBasic(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

dDowngradeToBasic :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
dDowngradeToBasic x0 r = uncurryFields  r $ dDowngradeToBasic' x0
   where
    dDowngradeToBasic' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    dDowngradeToBasic' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DDowngradeToBasicFn)

--------------------------------------------------------------------------------
-- | DSetArbitraryDataFn
--------------------------------------------------------------------------------


type DSetArbitraryDataFn = Tagged (SProxy "dSetArbitraryData(bytes32,bytes,bytes)") (Tuple3 (BytesN (D3 :& DOne D2)) ByteString ByteString)

dSetArbitraryData :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), key :: ByteString, value :: ByteString } -> Web3 e HexString
dSetArbitraryData x0 r = uncurryFields  r $ dSetArbitraryData' x0
   where
    dSetArbitraryData' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") ByteString -> Tagged (SProxy "value") ByteString -> Web3 e HexString
    dSetArbitraryData' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DSetArbitraryDataFn)

--------------------------------------------------------------------------------
-- | DOwnerErc20ClaimFn
--------------------------------------------------------------------------------


type DOwnerErc20ClaimFn = Tagged (SProxy "dOwnerErc20Claim(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

dOwnerErc20Claim :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
dOwnerErc20Claim x0 r = uncurryFields  r $ dOwnerErc20Claim' x0
   where
    dOwnerErc20Claim' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    dOwnerErc20Claim' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DOwnerErc20ClaimFn)

--------------------------------------------------------------------------------
-- | SetABackendFn
--------------------------------------------------------------------------------


type SetABackendFn = Tagged (SProxy "setABackend(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setABackend :: forall e. TransactionOptions NoPay -> { toSet :: (BytesN (D3 :& DOne D2)), newSC :: Address } -> Web3 e HexString
setABackend x0 r = uncurryFields  r $ setABackend' x0
   where
    setABackend' :: TransactionOptions NoPay -> Tagged (SProxy "toSet") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    setABackend' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetABackendFn)

--------------------------------------------------------------------------------
-- | GetUpgradePointerFn
--------------------------------------------------------------------------------


type GetUpgradePointerFn = Tagged (SProxy "getUpgradePointer()") (Tuple0 )

getUpgradePointer :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getUpgradePointer x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetUpgradePointerFn)

--------------------------------------------------------------------------------
-- | DDeployCommunityBallotFn
--------------------------------------------------------------------------------


type DDeployCommunityBallotFn = Tagged (SProxy "dDeployCommunityBallot(bytes32,bytes32,bytes32,uint128)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D1 :& D2 :& DOne D8)))

dDeployCommunityBallot :: forall e. TransactionOptions Wei -> { democHash :: (BytesN (D3 :& DOne D2)), specHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), packedTimes :: (UIntN (D1 :& D2 :& DOne D8)) } -> Web3 e HexString
dDeployCommunityBallot x0 r = uncurryFields  r $ dDeployCommunityBallot' x0
   where
    dDeployCommunityBallot' :: TransactionOptions Wei -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packedTimes") (UIntN (D1 :& D2 :& DOne D8)) -> Web3 e HexString
    dDeployCommunityBallot' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: DDeployCommunityBallotFn)

--------------------------------------------------------------------------------
-- | GetPaymentsFn
--------------------------------------------------------------------------------


type GetPaymentsFn = Tagged (SProxy "getPayments()") (Tuple0 )

getPayments :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getPayments x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentsFn)

--------------------------------------------------------------------------------
-- | GetBBFarmIDFn
--------------------------------------------------------------------------------


type GetBBFarmIDFn = Tagged (SProxy "getBBFarmID(bytes4)") (Tuple1 (BytesN (DOne D4)))

getBBFarmID :: forall e. TransactionOptions NoPay -> ChainCursor -> { bbNamespace :: (BytesN (DOne D4)) } -> Web3 e (Either CallError (UIntN (DOne D8)))
getBBFarmID x0 cm r = uncurryFields  r $ getBBFarmID' x0 cm
   where
    getBBFarmID' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "bbNamespace") (BytesN (DOne D4)) -> Web3 e (Either CallError (UIntN (DOne D8)))
    getBBFarmID' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetBBFarmIDFn)

--------------------------------------------------------------------------------
-- | SetDOwnerFn
--------------------------------------------------------------------------------


type SetDOwnerFn = Tagged (SProxy "setDOwner(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDOwner :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newOwner :: Address } -> Web3 e HexString
setDOwner x0 r = uncurryFields  r $ setDOwner' x0
   where
    setDOwner' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setDOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDOwnerFn)

--------------------------------------------------------------------------------
-- | DAddBallotFn
--------------------------------------------------------------------------------


type DAddBallotFn = Tagged (SProxy "dAddBallot(bytes32,uint256,uint256)") (Tuple3 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)))

dAddBallot :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), ballotId :: (UIntN (D2 :& D5 :& DOne D6)), packed :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dAddBallot x0 r = uncurryFields  r $ dAddBallot' x0
   where
    dAddBallot' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    dAddBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DAddBallotFn)

--------------------------------------------------------------------------------
-- | DUpgradeToPremiumFn
--------------------------------------------------------------------------------


type DUpgradeToPremiumFn = Tagged (SProxy "dUpgradeToPremium(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

dUpgradeToPremium :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
dUpgradeToPremium x0 r = uncurryFields  r $ dUpgradeToPremium' x0
   where
    dUpgradeToPremium' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    dUpgradeToPremium' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DUpgradeToPremiumFn)

--------------------------------------------------------------------------------
-- | DInitFn
--------------------------------------------------------------------------------


type DInitFn = Tagged (SProxy "dInit(address,bool)") (Tuple2 Address Boolean)

dInit :: forall e. TransactionOptions Wei -> { defaultErc20 :: Address, disableErc20OwnerClaim :: Boolean } -> Web3 e HexString
dInit x0 r = uncurryFields  r $ dInit' x0
   where
    dInit' :: TransactionOptions Wei -> Tagged (SProxy "defaultErc20") Address -> Tagged (SProxy "disableErc20OwnerClaim") Boolean -> Web3 e HexString
    dInit' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DInitFn)

--------------------------------------------------------------------------------
-- | GetCommAuctionFn
--------------------------------------------------------------------------------


type GetCommAuctionFn = Tagged (SProxy "getCommAuction()") (Tuple0 )

getCommAuction :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getCommAuction x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetCommAuctionFn)

--------------------------------------------------------------------------------
-- | SetDErc20Fn
--------------------------------------------------------------------------------


type SetDErc20Fn = Tagged (SProxy "setDErc20(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDErc20 :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newErc20 :: Address } -> Web3 e HexString
setDErc20 x0 r = uncurryFields  r $ setDErc20' x0
   where
    setDErc20' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newErc20") Address -> Web3 e HexString
    setDErc20' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDErc20Fn)

--------------------------------------------------------------------------------
-- | SetDEditorFn
--------------------------------------------------------------------------------


type SetDEditorFn = Tagged (SProxy "setDEditor(bytes32,address,bool)") (Tuple3 (BytesN (D3 :& DOne D2)) Address Boolean)

setDEditor :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), editor :: Address, canEdit :: Boolean } -> Web3 e HexString
setDEditor x0 r = uncurryFields  r $ setDEditor' x0
   where
    setDEditor' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "editor") Address -> Tagged (SProxy "canEdit") Boolean -> Web3 e HexString
    setDEditor' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SetDEditorFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address,address,address,address)") (Tuple5 Address Address Address Address Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _b :: Address, _pay :: Address, _ensOwnerPx :: Address, _bbFarm0 :: Address, _commAuction :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_b") Address -> Tagged (SProxy "_pay") Address -> Tagged (SProxy "_ensOwnerPx") Address -> Tagged (SProxy "_bbFarm0") Address -> Tagged (SProxy "_commAuction") Address -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 y5 y6 = deployContract y0 bc' ((tagged $ Tuple5 (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 ) (untagged y6 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | PayoutAll
--------------------------------------------------------------------------------


newtype PayoutAll = PayoutAll {payTo :: Address,value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePayoutAll :: Newtype PayoutAll _

instance eventFilterPayoutAll :: EventFilter PayoutAll where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e2644f8d6fd3207ea14ef6a361b94bee348c8e5834539376241010dbd2562472")]

instance indexedEventPayoutAll :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "payTo") Address) (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) PayoutAll where
  isAnonymous _ = false

derive instance genericPayoutAll :: Generic PayoutAll _

instance eventGenericPayoutAllShow :: Show PayoutAll where
	show = genericShow

instance eventGenericPayoutAlleq :: Eq PayoutAll where
	eq = genericEq

--------------------------------------------------------------------------------
-- | OwnerChanged
--------------------------------------------------------------------------------


newtype OwnerChanged = OwnerChanged {newOwner :: Address}

derive instance newtypeOwnerChanged :: Newtype OwnerChanged _

instance eventFilterOwnerChanged :: EventFilter OwnerChanged where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a2ea9883a321a3e97b8266c2b078bfeec6d50c711ed71f874a90d500ae2eaf36")]

instance indexedEventOwnerChanged :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "newOwner") Address)) OwnerChanged where
  isAnonymous _ = false

derive instance genericOwnerChanged :: Generic OwnerChanged _

instance eventGenericOwnerChangedShow :: Show OwnerChanged where
	show = genericShow

instance eventGenericOwnerChangedeq :: Eq OwnerChanged where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PaymentMade
--------------------------------------------------------------------------------


newtype PaymentMade = PaymentMade {valAndRemainder :: (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))}

derive instance newtypePaymentMade :: Newtype PaymentMade _

instance eventFilterPaymentMade :: EventFilter PaymentMade where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a10b8a9911b680d93c2cbb993f5f5a4921bb52ebcb7926bf777c3831a5a1f30b")]

instance indexedEventPaymentMade :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "valAndRemainder") (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))))) PaymentMade where
  isAnonymous _ = false

derive instance genericPaymentMade :: Generic PaymentMade _

instance eventGenericPaymentMadeShow :: Show PaymentMade where
	show = genericShow

instance eventGenericPaymentMadeeq :: Eq PaymentMade where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AddedBBFarm
--------------------------------------------------------------------------------


newtype AddedBBFarm = AddedBBFarm {bbFarmId :: (UIntN (DOne D8))}

derive instance newtypeAddedBBFarm :: Newtype AddedBBFarm _

instance eventFilterAddedBBFarm :: EventFilter AddedBBFarm where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8ea210065077b12c19240bd91ee2acbce0fd74690be559681ca116194e1ec98a")]

instance indexedEventAddedBBFarm :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "bbFarmId") (UIntN (DOne D8)))) AddedBBFarm where
  isAnonymous _ = false

derive instance genericAddedBBFarm :: Generic AddedBBFarm _

instance eventGenericAddedBBFarmShow :: Show AddedBBFarm where
	show = genericShow

instance eventGenericAddedBBFarmeq :: Eq AddedBBFarm where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetBackend
--------------------------------------------------------------------------------


newtype SetBackend = SetBackend {setWhat :: (BytesN (D3 :& DOne D2)),newSC :: Address}

derive instance newtypeSetBackend :: Newtype SetBackend _

instance eventFilterSetBackend :: EventFilter SetBackend where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c60d12c3d3da7d885eec112ff6fb3ef9a77dcaa4727fe822eb8e9bcd881c0f1b")]

instance indexedEventSetBackend :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "setWhat") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "newSC") Address)) SetBackend where
  isAnonymous _ = false

derive instance genericSetBackend :: Generic SetBackend _

instance eventGenericSetBackendShow :: Show SetBackend where
	show = genericShow

instance eventGenericSetBackendeq :: Eq SetBackend where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DeprecatedBBFarm
--------------------------------------------------------------------------------


newtype DeprecatedBBFarm = DeprecatedBBFarm {bbFarmId :: (UIntN (DOne D8))}

derive instance newtypeDeprecatedBBFarm :: Newtype DeprecatedBBFarm _

instance eventFilterDeprecatedBBFarm :: EventFilter DeprecatedBBFarm where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8e83bd35628184fa0608ea0dfe3901aeb6bcdc16c997d4d7cc1ee71becf0458d")]

instance indexedEventDeprecatedBBFarm :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "bbFarmId") (UIntN (DOne D8)))) DeprecatedBBFarm where
  isAnonymous _ = false

derive instance genericDeprecatedBBFarm :: Generic DeprecatedBBFarm _

instance eventGenericDeprecatedBBFarmShow :: Show DeprecatedBBFarm where
	show = genericShow

instance eventGenericDeprecatedBBFarmeq :: Eq DeprecatedBBFarm where
	eq = genericEq

--------------------------------------------------------------------------------
-- | CommunityBallot
--------------------------------------------------------------------------------


newtype CommunityBallot = CommunityBallot {democHash :: (BytesN (D3 :& DOne D2)),ballotId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeCommunityBallot :: Newtype CommunityBallot _

instance eventFilterCommunityBallot :: EventFilter CommunityBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "5a0113260875225c161819294ea7f36e73226e046f98fac0ce9ea2af3ca6f028")]

instance indexedEventCommunityBallot :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)))) CommunityBallot where
  isAnonymous _ = false

derive instance genericCommunityBallot :: Generic CommunityBallot _

instance eventGenericCommunityBallotShow :: Show CommunityBallot where
	show = genericShow

instance eventGenericCommunityBalloteq :: Eq CommunityBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ManuallyAddedBallot
--------------------------------------------------------------------------------


newtype ManuallyAddedBallot = ManuallyAddedBallot {democHash :: (BytesN (D3 :& DOne D2)),ballotId :: (UIntN (D2 :& D5 :& DOne D6)),packed :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeManuallyAddedBallot :: Newtype ManuallyAddedBallot _

instance eventFilterManuallyAddedBallot :: EventFilter ManuallyAddedBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "17b6c47e4b3b8370c0983be04968d8d3ae4e3a04b369ac06b0492796ef194ea3")]

instance indexedEventManuallyAddedBallot :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)))) ManuallyAddedBallot where
  isAnonymous _ = false

derive instance genericManuallyAddedBallot :: Generic ManuallyAddedBallot _

instance eventGenericManuallyAddedBallotShow :: Show ManuallyAddedBallot where
	show = genericShow

instance eventGenericManuallyAddedBalloteq :: Eq ManuallyAddedBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BallotCreatedWithID
--------------------------------------------------------------------------------


newtype BallotCreatedWithID = BallotCreatedWithID {ballotId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBallotCreatedWithID :: Newtype BallotCreatedWithID _

instance eventFilterBallotCreatedWithID :: EventFilter BallotCreatedWithID where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "20f1b9a21ee397f1c57261849e4492865559e3da426f13a27e9d3abefafb45ed")]

instance indexedEventBallotCreatedWithID :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)))) BallotCreatedWithID where
  isAnonymous _ = false

derive instance genericBallotCreatedWithID :: Generic BallotCreatedWithID _

instance eventGenericBallotCreatedWithIDShow :: Show BallotCreatedWithID where
	show = genericShow

instance eventGenericBallotCreatedWithIDeq :: Eq BallotCreatedWithID where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BBFarmInit
--------------------------------------------------------------------------------


newtype BBFarmInit = BBFarmInit {namespace :: (BytesN (DOne D4))}

derive instance newtypeBBFarmInit :: Newtype BBFarmInit _

instance eventFilterBBFarmInit :: EventFilter BBFarmInit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "9efcb9c0754671258cec21b6dce843609343e2240774fedbc3a062d6d79ed0f8")]

instance indexedEventBBFarmInit :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "namespace") (BytesN (DOne D4)))) BBFarmInit where
  isAnonymous _ = false

derive instance genericBBFarmInit :: Generic BBFarmInit _

instance eventGenericBBFarmInitShow :: Show BBFarmInit where
	show = genericShow

instance eventGenericBBFarmIniteq :: Eq BBFarmInit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | NewDemoc
--------------------------------------------------------------------------------


newtype NewDemoc = NewDemoc {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeNewDemoc :: Newtype NewDemoc _

instance eventFilterNewDemoc :: EventFilter NewDemoc where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0c20027e6e14b382bc4c0eb23bbb44394e15081383d9088dd44adcac951830fa")]

instance indexedEventNewDemoc :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) NewDemoc where
  isAnonymous _ = false

derive instance genericNewDemoc :: Generic NewDemoc _

instance eventGenericNewDemocShow :: Show NewDemoc where
	show = genericShow

instance eventGenericNewDemoceq :: Eq NewDemoc where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ManuallyAddedDemoc
--------------------------------------------------------------------------------


newtype ManuallyAddedDemoc = ManuallyAddedDemoc {democHash :: (BytesN (D3 :& DOne D2)),erc20 :: Address}

derive instance newtypeManuallyAddedDemoc :: Newtype ManuallyAddedDemoc _

instance eventFilterManuallyAddedDemoc :: EventFilter ManuallyAddedDemoc where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "76cfde4818906d3ebeda8d4aa81b23c066bbb956bb70274c4126ec9c0faead4e")]

instance indexedEventManuallyAddedDemoc :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "erc20") Address)) ManuallyAddedDemoc where
  isAnonymous _ = false

derive instance genericManuallyAddedDemoc :: Generic ManuallyAddedDemoc _

instance eventGenericManuallyAddedDemocShow :: Show ManuallyAddedDemoc where
	show = genericShow

instance eventGenericManuallyAddedDemoceq :: Eq ManuallyAddedDemoc where
	eq = genericEq

--------------------------------------------------------------------------------
-- | NewBallot
--------------------------------------------------------------------------------


newtype NewBallot = NewBallot {democHash :: (BytesN (D3 :& DOne D2)),ballotN :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeNewBallot :: Newtype NewBallot _

instance eventFilterNewBallot :: EventFilter NewBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "73da1acab1521244521194424efac3de42f8bb8aa8c3b4f85843d4bfb7b3df84"),Nothing]

instance indexedEventNewBallot :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "ballotN") (UIntN (D2 :& D5 :& DOne D6)))) NewBallot where
  isAnonymous _ = false

derive instance genericNewBallot :: Generic NewBallot _

instance eventGenericNewBallotShow :: Show NewBallot where
	show = genericShow

instance eventGenericNewBalloteq :: Eq NewBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocOwnerSet
--------------------------------------------------------------------------------


newtype DemocOwnerSet = DemocOwnerSet {democHash :: (BytesN (D3 :& DOne D2)),owner :: Address}

derive instance newtypeDemocOwnerSet :: Newtype DemocOwnerSet _

instance eventFilterDemocOwnerSet :: EventFilter DemocOwnerSet where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "61b9d2862914325dd13bcfe9fa63019518a298b091322a499110806cc8392149"),Nothing]

instance indexedEventDemocOwnerSet :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "owner") Address)) DemocOwnerSet where
  isAnonymous _ = false

derive instance genericDemocOwnerSet :: Generic DemocOwnerSet _

instance eventGenericDemocOwnerSetShow :: Show DemocOwnerSet where
	show = genericShow

instance eventGenericDemocOwnerSeteq :: Eq DemocOwnerSet where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocEditorSet
--------------------------------------------------------------------------------


newtype DemocEditorSet = DemocEditorSet {democHash :: (BytesN (D3 :& DOne D2)),editor :: Address,canEdit :: Boolean}

derive instance newtypeDemocEditorSet :: Newtype DemocEditorSet _

instance eventFilterDemocEditorSet :: EventFilter DemocEditorSet where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e4ab708eaba56e3fc86c807c52597fa3912723db2c4b47704e8e3729533154a6"),Nothing]

instance indexedEventDemocEditorSet :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple2 (Tagged (SProxy "editor") Address) (Tagged (SProxy "canEdit") Boolean)) DemocEditorSet where
  isAnonymous _ = false

derive instance genericDemocEditorSet :: Generic DemocEditorSet _

instance eventGenericDemocEditorSetShow :: Show DemocEditorSet where
	show = genericShow

instance eventGenericDemocEditorSeteq :: Eq DemocEditorSet where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocEditorsWiped
--------------------------------------------------------------------------------


newtype DemocEditorsWiped = DemocEditorsWiped {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeDemocEditorsWiped :: Newtype DemocEditorsWiped _

instance eventFilterDemocEditorsWiped :: EventFilter DemocEditorsWiped where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f3bbc8c454575f9f6aa4d88c305beb91199abb12ec76ce392bc593fc771e3fba"),Nothing]

instance indexedEventDemocEditorsWiped :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) DemocEditorsWiped where
  isAnonymous _ = false

derive instance genericDemocEditorsWiped :: Generic DemocEditorsWiped _

instance eventGenericDemocEditorsWipedShow :: Show DemocEditorsWiped where
	show = genericShow

instance eventGenericDemocEditorsWipedeq :: Eq DemocEditorsWiped where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocErc20Set
--------------------------------------------------------------------------------


newtype DemocErc20Set = DemocErc20Set {democHash :: (BytesN (D3 :& DOne D2)),erc20 :: Address}

derive instance newtypeDemocErc20Set :: Newtype DemocErc20Set _

instance eventFilterDemocErc20Set :: EventFilter DemocErc20Set where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "9db922c152f2e7c530d60219abb1e50af8b8a6db04e369ba9ed99d54126b0966"),Nothing]

instance indexedEventDemocErc20Set :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "erc20") Address)) DemocErc20Set where
  isAnonymous _ = false

derive instance genericDemocErc20Set :: Generic DemocErc20Set _

instance eventGenericDemocErc20SetShow :: Show DemocErc20Set where
	show = genericShow

instance eventGenericDemocErc20Seteq :: Eq DemocErc20Set where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocDataSet
--------------------------------------------------------------------------------


newtype DemocDataSet = DemocDataSet {democHash :: (BytesN (D3 :& DOne D2)),keyHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeDemocDataSet :: Newtype DemocDataSet _

instance eventFilterDemocDataSet :: EventFilter DemocDataSet where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "70cd3b363a8374b507c0ea17f9205a693a37d89c14d1391d4a9d8d3509f6dbb0"),Nothing]

instance indexedEventDemocDataSet :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "keyHash") (BytesN (D3 :& DOne D2)))) DemocDataSet where
  isAnonymous _ = false

derive instance genericDemocDataSet :: Generic DemocDataSet _

instance eventGenericDemocDataSetShow :: Show DemocDataSet where
	show = genericShow

instance eventGenericDemocDataSeteq :: Eq DemocDataSet where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocCatAdded
--------------------------------------------------------------------------------


newtype DemocCatAdded = DemocCatAdded {democHash :: (BytesN (D3 :& DOne D2)),catId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeDemocCatAdded :: Newtype DemocCatAdded _

instance eventFilterDemocCatAdded :: EventFilter DemocCatAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b00e174944b708eb07afc8afcdcc94d5dfa6c1246743602a8ce6a5bedad2f4e5"),Nothing]

instance indexedEventDemocCatAdded :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "catId") (UIntN (D2 :& D5 :& DOne D6)))) DemocCatAdded where
  isAnonymous _ = false

derive instance genericDemocCatAdded :: Generic DemocCatAdded _

instance eventGenericDemocCatAddedShow :: Show DemocCatAdded where
	show = genericShow

instance eventGenericDemocCatAddedeq :: Eq DemocCatAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocCatDeprecated
--------------------------------------------------------------------------------


newtype DemocCatDeprecated = DemocCatDeprecated {democHash :: (BytesN (D3 :& DOne D2)),catId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeDemocCatDeprecated :: Newtype DemocCatDeprecated _

instance eventFilterDemocCatDeprecated :: EventFilter DemocCatDeprecated where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "bcffb42fe43e03fdde1eca40d96bb1e9e48eff8d3ec71e025b2304977e64f392"),Nothing]

instance indexedEventDemocCatDeprecated :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "catId") (UIntN (D2 :& D5 :& DOne D6)))) DemocCatDeprecated where
  isAnonymous _ = false

derive instance genericDemocCatDeprecated :: Generic DemocCatDeprecated _

instance eventGenericDemocCatDeprecatedShow :: Show DemocCatDeprecated where
	show = genericShow

instance eventGenericDemocCatDeprecatedeq :: Eq DemocCatDeprecated where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocCommunityBallotsEnabled
--------------------------------------------------------------------------------


newtype DemocCommunityBallotsEnabled = DemocCommunityBallotsEnabled {democHash :: (BytesN (D3 :& DOne D2)),enabled :: Boolean}

derive instance newtypeDemocCommunityBallotsEnabled :: Newtype DemocCommunityBallotsEnabled _

instance eventFilterDemocCommunityBallotsEnabled :: EventFilter DemocCommunityBallotsEnabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "93b729dac8f3963db35b89b16922310952f9bf8b37e391b99218c03faa42f3f4"),Nothing]

instance indexedEventDemocCommunityBallotsEnabled :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "enabled") Boolean)) DemocCommunityBallotsEnabled where
  isAnonymous _ = false

derive instance genericDemocCommunityBallotsEnabled :: Generic DemocCommunityBallotsEnabled _

instance eventGenericDemocCommunityBallotsEnabledShow :: Show DemocCommunityBallotsEnabled where
	show = genericShow

instance eventGenericDemocCommunityBallotsEnabledeq :: Eq DemocCommunityBallotsEnabled where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocErc20OwnerClaimDisabled
--------------------------------------------------------------------------------


newtype DemocErc20OwnerClaimDisabled = DemocErc20OwnerClaimDisabled {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeDemocErc20OwnerClaimDisabled :: Newtype DemocErc20OwnerClaimDisabled _

instance eventFilterDemocErc20OwnerClaimDisabled :: EventFilter DemocErc20OwnerClaimDisabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "cc23847a8a66577fe576136f5f34967fbd9cf71d95b0c90647cea48bc236d57d"),Nothing]

instance indexedEventDemocErc20OwnerClaimDisabled :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) DemocErc20OwnerClaimDisabled where
  isAnonymous _ = false

derive instance genericDemocErc20OwnerClaimDisabled :: Generic DemocErc20OwnerClaimDisabled _

instance eventGenericDemocErc20OwnerClaimDisabledShow :: Show DemocErc20OwnerClaimDisabled where
	show = genericShow

instance eventGenericDemocErc20OwnerClaimDisabledeq :: Eq DemocErc20OwnerClaimDisabled where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DemocClaimed
--------------------------------------------------------------------------------


newtype DemocClaimed = DemocClaimed {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeDemocClaimed :: Newtype DemocClaimed _

instance eventFilterDemocClaimed :: EventFilter DemocClaimed where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3a20afcbf909577cfdda0121a1a06de3ba63d5ccd34b0653920cffcf93dd6935"),Nothing]

instance indexedEventDemocClaimed :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) DemocClaimed where
  isAnonymous _ = false

derive instance genericDemocClaimed :: Generic DemocClaimed _

instance eventGenericDemocClaimedShow :: Show DemocClaimed where
	show = genericShow

instance eventGenericDemocClaimedeq :: Eq DemocClaimed where
	eq = genericEq

--------------------------------------------------------------------------------
-- | EmergencyDemocOwner
--------------------------------------------------------------------------------


newtype EmergencyDemocOwner = EmergencyDemocOwner {democHash :: (BytesN (D3 :& DOne D2)),newOwner :: Address}

derive instance newtypeEmergencyDemocOwner :: Newtype EmergencyDemocOwner _

instance eventFilterEmergencyDemocOwner :: EventFilter EmergencyDemocOwner where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "85644e2e7afe6ef9f59ba824a1a8b530832a9fb9f73052bc316f5168a156505a"),Nothing]

instance indexedEventEmergencyDemocOwner :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "newOwner") Address)) EmergencyDemocOwner where
  isAnonymous _ = false

derive instance genericEmergencyDemocOwner :: Generic EmergencyDemocOwner _

instance eventGenericEmergencyDemocOwnerShow :: Show EmergencyDemocOwner where
	show = genericShow

instance eventGenericEmergencyDemocOwnereq :: Eq EmergencyDemocOwner where
	eq = genericEq

--------------------------------------------------------------------------------
-- | UpgradedToPremium
--------------------------------------------------------------------------------


newtype UpgradedToPremium = UpgradedToPremium {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeUpgradedToPremium :: Newtype UpgradedToPremium _

instance eventFilterUpgradedToPremium :: EventFilter UpgradedToPremium where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b957c6a4668e76eb2e541f54eff529717f8070dba0ef7813dd8ab28cbbf0f702"),Nothing]

instance indexedEventUpgradedToPremium :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) UpgradedToPremium where
  isAnonymous _ = false

derive instance genericUpgradedToPremium :: Generic UpgradedToPremium _

instance eventGenericUpgradedToPremiumShow :: Show UpgradedToPremium where
	show = genericShow

instance eventGenericUpgradedToPremiumeq :: Eq UpgradedToPremium where
	eq = genericEq

--------------------------------------------------------------------------------
-- | GrantedAccountTime
--------------------------------------------------------------------------------


newtype GrantedAccountTime = GrantedAccountTime {democHash :: (BytesN (D3 :& DOne D2)),additionalSeconds :: (UIntN (D2 :& D5 :& DOne D6)),ref :: (BytesN (D3 :& DOne D2))}

derive instance newtypeGrantedAccountTime :: Newtype GrantedAccountTime _

instance eventFilterGrantedAccountTime :: EventFilter GrantedAccountTime where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "662d736f3a0c8f3b0906b173689606e590f214abef4ae9bde801e1b6f4059f4d"),Nothing]

instance indexedEventGrantedAccountTime :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple2 (Tagged (SProxy "additionalSeconds") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "ref") (BytesN (D3 :& DOne D2)))) GrantedAccountTime where
  isAnonymous _ = false

derive instance genericGrantedAccountTime :: Generic GrantedAccountTime _

instance eventGenericGrantedAccountTimeShow :: Show GrantedAccountTime where
	show = genericShow

instance eventGenericGrantedAccountTimeeq :: Eq GrantedAccountTime where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AccountPayment
--------------------------------------------------------------------------------


newtype AccountPayment = AccountPayment {democHash :: (BytesN (D3 :& DOne D2)),additionalSeconds :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAccountPayment :: Newtype AccountPayment _

instance eventFilterAccountPayment :: EventFilter AccountPayment where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e6ad0535314a8031f23d28b2c0945848329bf43d9d12471ce4f5f99f66406576"),Nothing]

instance indexedEventAccountPayment :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "additionalSeconds") (UIntN (D2 :& D5 :& DOne D6)))) AccountPayment where
  isAnonymous _ = false

derive instance genericAccountPayment :: Generic AccountPayment _

instance eventGenericAccountPaymentShow :: Show AccountPayment where
	show = genericShow

instance eventGenericAccountPaymenteq :: Eq AccountPayment where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetCommunityBallotFee
--------------------------------------------------------------------------------


newtype SetCommunityBallotFee = SetCommunityBallotFee {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetCommunityBallotFee :: Newtype SetCommunityBallotFee _

instance eventFilterSetCommunityBallotFee :: EventFilter SetCommunityBallotFee where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "527bf3f500392f0f845f9c3c2b3a6b253d7d34472e90e894b25eed6e7dd79b7f")]

instance indexedEventSetCommunityBallotFee :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetCommunityBallotFee where
  isAnonymous _ = false

derive instance genericSetCommunityBallotFee :: Generic SetCommunityBallotFee _

instance eventGenericSetCommunityBallotFeeShow :: Show SetCommunityBallotFee where
	show = genericShow

instance eventGenericSetCommunityBallotFeeeq :: Eq SetCommunityBallotFee where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetBasicCentsPricePer30Days
--------------------------------------------------------------------------------


newtype SetBasicCentsPricePer30Days = SetBasicCentsPricePer30Days {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetBasicCentsPricePer30Days :: Newtype SetBasicCentsPricePer30Days _

instance eventFilterSetBasicCentsPricePer30Days :: EventFilter SetBasicCentsPricePer30Days where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "696e5c6fec16a11a25b2139177dc42dbfe83df4fd078f5a7f13b6db57792d1a1")]

instance indexedEventSetBasicCentsPricePer30Days :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetBasicCentsPricePer30Days where
  isAnonymous _ = false

derive instance genericSetBasicCentsPricePer30Days :: Generic SetBasicCentsPricePer30Days _

instance eventGenericSetBasicCentsPricePer30DaysShow :: Show SetBasicCentsPricePer30Days where
	show = genericShow

instance eventGenericSetBasicCentsPricePer30Dayseq :: Eq SetBasicCentsPricePer30Days where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetPremiumMultiplier
--------------------------------------------------------------------------------


newtype SetPremiumMultiplier = SetPremiumMultiplier {multiplier :: (UIntN (DOne D8))}

derive instance newtypeSetPremiumMultiplier :: Newtype SetPremiumMultiplier _

instance eventFilterSetPremiumMultiplier :: EventFilter SetPremiumMultiplier where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "dd5e68e86e7460257caaf7a2276fa63562e9a906a39802c4f37699f5736a72f2")]

instance indexedEventSetPremiumMultiplier :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "multiplier") (UIntN (DOne D8)))) SetPremiumMultiplier where
  isAnonymous _ = false

derive instance genericSetPremiumMultiplier :: Generic SetPremiumMultiplier _

instance eventGenericSetPremiumMultiplierShow :: Show SetPremiumMultiplier where
	show = genericShow

instance eventGenericSetPremiumMultipliereq :: Eq SetPremiumMultiplier where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DowngradeToBasic
--------------------------------------------------------------------------------


newtype DowngradeToBasic = DowngradeToBasic {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeDowngradeToBasic :: Newtype DowngradeToBasic _

instance eventFilterDowngradeToBasic :: EventFilter DowngradeToBasic where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "48f962cb175066eb5227bf2d28122d1f0861b74cd6951e6542a6f0283b6a30b7"),Nothing]

instance indexedEventDowngradeToBasic :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) DowngradeToBasic where
  isAnonymous _ = false

derive instance genericDowngradeToBasic :: Generic DowngradeToBasic _

instance eventGenericDowngradeToBasicShow :: Show DowngradeToBasic where
	show = genericShow

instance eventGenericDowngradeToBasiceq :: Eq DowngradeToBasic where
	eq = genericEq

--------------------------------------------------------------------------------
-- | UpgradeToPremium
--------------------------------------------------------------------------------


newtype UpgradeToPremium = UpgradeToPremium {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeUpgradeToPremium :: Newtype UpgradeToPremium _

instance eventFilterUpgradeToPremium :: EventFilter UpgradeToPremium where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ac98838319e78f6dc944f503ff87fffb262f5a9a6360e961a802837072b81e0f"),Nothing]

instance indexedEventUpgradeToPremium :: IndexedEvent (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) (Tuple0 ) UpgradeToPremium where
  isAnonymous _ = false

derive instance genericUpgradeToPremium :: Generic UpgradeToPremium _

instance eventGenericUpgradeToPremiumShow :: Show UpgradeToPremium where
	show = genericShow

instance eventGenericUpgradeToPremiumeq :: Eq UpgradeToPremium where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetExchangeRate
--------------------------------------------------------------------------------


newtype SetExchangeRate = SetExchangeRate {weiPerCent :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetExchangeRate :: Newtype SetExchangeRate _

instance eventFilterSetExchangeRate :: EventFilter SetExchangeRate where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1d5de90e7c5b244ac5797698b15fe80a92524d933dafd79e001daf844555fb1c")]

instance indexedEventSetExchangeRate :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "weiPerCent") (UIntN (D2 :& D5 :& DOne D6)))) SetExchangeRate where
  isAnonymous _ = false

derive instance genericSetExchangeRate :: Generic SetExchangeRate _

instance eventGenericSetExchangeRateShow :: Show SetExchangeRate where
	show = genericShow

instance eventGenericSetExchangeRateeq :: Eq SetExchangeRate where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FreeExtension
--------------------------------------------------------------------------------


newtype FreeExtension = FreeExtension {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeFreeExtension :: Newtype FreeExtension _

instance eventFilterFreeExtension :: EventFilter FreeExtension where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "09664427434c2b4309000ea03b5e378fa25c30acbe0a04fdeff36061303f8e7e")]

instance indexedEventFreeExtension :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) FreeExtension where
  isAnonymous _ = false

derive instance genericFreeExtension :: Generic FreeExtension _

instance eventGenericFreeExtensionShow :: Show FreeExtension where
	show = genericShow

instance eventGenericFreeExtensioneq :: Eq FreeExtension where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetBallotsPer30Days
--------------------------------------------------------------------------------


newtype SetBallotsPer30Days = SetBallotsPer30Days {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetBallotsPer30Days :: Newtype SetBallotsPer30Days _

instance eventFilterSetBallotsPer30Days :: EventFilter SetBallotsPer30Days where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "cfa56694fbda1b84c5d6c63fe8f6874c83c94f2da04231dd0d54e07cb542e62f")]

instance indexedEventSetBallotsPer30Days :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetBallotsPer30Days where
  isAnonymous _ = false

derive instance genericSetBallotsPer30Days :: Generic SetBallotsPer30Days _

instance eventGenericSetBallotsPer30DaysShow :: Show SetBallotsPer30Days where
	show = genericShow

instance eventGenericSetBallotsPer30Dayseq :: Eq SetBallotsPer30Days where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetFreeExtension
--------------------------------------------------------------------------------


newtype SetFreeExtension = SetFreeExtension {democHash :: (BytesN (D3 :& DOne D2)),hasFreeExt :: Boolean}

derive instance newtypeSetFreeExtension :: Newtype SetFreeExtension _

instance eventFilterSetFreeExtension :: EventFilter SetFreeExtension where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "de965928e65e33ccd6559d861e5483e90512b32455de3d5eec039b9a5b93f8e4")]

instance indexedEventSetFreeExtension :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "hasFreeExt") Boolean)) SetFreeExtension where
  isAnonymous _ = false

derive instance genericSetFreeExtension :: Generic SetFreeExtension _

instance eventGenericSetFreeExtensionShow :: Show SetFreeExtension where
	show = genericShow

instance eventGenericSetFreeExtensioneq :: Eq SetFreeExtension where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetDenyPremium
--------------------------------------------------------------------------------


newtype SetDenyPremium = SetDenyPremium {democHash :: (BytesN (D3 :& DOne D2)),isPremiumDenied :: Boolean}

derive instance newtypeSetDenyPremium :: Newtype SetDenyPremium _

instance eventFilterSetDenyPremium :: EventFilter SetDenyPremium where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "50935952ae272746cf38a1cb4aa7c93d3e39e60cdc4a761ef55a4e69366abe7d")]

instance indexedEventSetDenyPremium :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "isPremiumDenied") Boolean)) SetDenyPremium where
  isAnonymous _ = false

derive instance genericSetDenyPremium :: Generic SetDenyPremium _

instance eventGenericSetDenyPremiumShow :: Show SetDenyPremium where
	show = genericShow

instance eventGenericSetDenyPremiumeq :: Eq SetDenyPremium where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetPayTo
--------------------------------------------------------------------------------


newtype SetPayTo = SetPayTo {payTo :: Address}

derive instance newtypeSetPayTo :: Newtype SetPayTo _

instance eventFilterSetPayTo :: EventFilter SetPayTo where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "bea0769aba15b443dd3170dc4bc7b94123881e42d6ed3d31a2654daf43b05851")]

instance indexedEventSetPayTo :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "payTo") Address)) SetPayTo where
  isAnonymous _ = false

derive instance genericSetPayTo :: Generic SetPayTo _

instance eventGenericSetPayToShow :: Show SetPayTo where
	show = genericShow

instance eventGenericSetPayToeq :: Eq SetPayTo where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetMinorEditsAddr
--------------------------------------------------------------------------------


newtype SetMinorEditsAddr = SetMinorEditsAddr {minorEditsAddr :: Address}

derive instance newtypeSetMinorEditsAddr :: Newtype SetMinorEditsAddr _

instance eventFilterSetMinorEditsAddr :: EventFilter SetMinorEditsAddr where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d72f7e1ee158c792883f68eb79af23661439288dae047b5a0f1fb6c8cd884973")]

instance indexedEventSetMinorEditsAddr :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "minorEditsAddr") Address)) SetMinorEditsAddr where
  isAnonymous _ = false

derive instance genericSetMinorEditsAddr :: Generic SetMinorEditsAddr _

instance eventGenericSetMinorEditsAddrShow :: Show SetMinorEditsAddr where
	show = genericShow

instance eventGenericSetMinorEditsAddreq :: Eq SetMinorEditsAddr where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SetMinWeiForDInit
--------------------------------------------------------------------------------


newtype SetMinWeiForDInit = SetMinWeiForDInit {amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetMinWeiForDInit :: Newtype SetMinWeiForDInit _

instance eventFilterSetMinWeiForDInit :: EventFilter SetMinWeiForDInit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "134bc3fbd032443af0856b9e13b4146103106e96c9c095d10415247f5369c4cf")]

instance indexedEventSetMinWeiForDInit :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SetMinWeiForDInit where
  isAnonymous _ = false

derive instance genericSetMinWeiForDInit :: Generic SetMinWeiForDInit _

instance eventGenericSetMinWeiForDInitShow :: Show SetMinWeiForDInit where
	show = genericShow

instance eventGenericSetMinWeiForDIniteq :: Eq SetMinWeiForDInit where
	eq = genericEq