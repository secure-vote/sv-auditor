--------------------------------------------------------------------------------
-- | SVLightIndex
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVLightIndex where

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
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, Wei, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
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
-- | GetDCategoriesNFn
--------------------------------------------------------------------------------


type GetDCategoriesNFn = Tagged (SProxy "getDCategoriesN(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDCategoriesN :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDCategoriesN x0 cm r = uncurryFields  r $ getDCategoriesN' x0 cm
   where
    getDCategoriesN' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDCategoriesN' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDCategoriesNFn)

--------------------------------------------------------------------------------
-- | GetDBallotBoxFn
--------------------------------------------------------------------------------


type GetDBallotBoxFn = Tagged (SProxy "getDBallotBox(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDBallotBox :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getDBallotBox x0 cm r = uncurryFields  r $ getDBallotBox' x0 cm
   where
    getDBallotBox' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getDBallotBox' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDBallotBoxFn)

--------------------------------------------------------------------------------
-- | DDeprecateCategoryFn
--------------------------------------------------------------------------------


type DDeprecateCategoryFn = Tagged (SProxy "dDeprecateCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

dDeprecateCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), categoryId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dDeprecateCategory x0 r = uncurryFields  r $ dDeprecateCategory' x0
   where
    dDeprecateCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    dDeprecateCategory' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DDeprecateCategoryFn)

--------------------------------------------------------------------------------
-- | BackendFn
--------------------------------------------------------------------------------


type BackendFn = Tagged (SProxy "backend()") (Tuple0 )

backend :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
backend x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BackendFn)

--------------------------------------------------------------------------------
-- | GetVersionFn
--------------------------------------------------------------------------------


type GetVersionFn = Tagged (SProxy "getVersion()") (Tuple0 )

getVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetVersionFn)

--------------------------------------------------------------------------------
-- | DInitFn
--------------------------------------------------------------------------------


type DInitFn = Tagged (SProxy "dInit(address)") (Tuple1 Address)

dInit :: forall e. TransactionOptions Wei -> { defaultErc20 :: Address } -> Web3 e HexString
dInit x0 r = uncurryFields  r $ dInit' x0
   where
    dInit' :: TransactionOptions Wei -> Tagged (SProxy "defaultErc20") Address -> Web3 e HexString
    dInit' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DInitFn)

--------------------------------------------------------------------------------
-- | GetCommunityBallotCentsPriceFn
--------------------------------------------------------------------------------


type GetCommunityBallotCentsPriceFn = Tagged (SProxy "getCommunityBallotCentsPrice()") (Tuple0 )

getCommunityBallotCentsPrice :: forall e. TransactionOptions NoPay -> Web3 e HexString
getCommunityBallotCentsPrice x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetCommunityBallotCentsPriceFn)

--------------------------------------------------------------------------------
-- | AdminPxFactoryFn
--------------------------------------------------------------------------------


type AdminPxFactoryFn = Tagged (SProxy "adminPxFactory()") (Tuple0 )

adminPxFactory :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
adminPxFactory x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminPxFactoryFn)

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
-- | GetGDemocFn
--------------------------------------------------------------------------------


type GetGDemocFn = Tagged (SProxy "getGDemoc(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getGDemoc :: forall e. TransactionOptions NoPay -> ChainCursor -> { n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getGDemoc x0 cm r = uncurryFields  r $ getGDemoc' x0 cm
   where
    getGDemoc' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getGDemoc' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetGDemocFn)

--------------------------------------------------------------------------------
-- | SetPaymentBackendFn
--------------------------------------------------------------------------------


type SetPaymentBackendFn = Tagged (SProxy "setPaymentBackend(address)") (Tuple1 Address)

setPaymentBackend :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
setPaymentBackend x0 r = uncurryFields  r $ setPaymentBackend' x0
   where
    setPaymentBackend' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    setPaymentBackend' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetPaymentBackendFn)

--------------------------------------------------------------------------------
-- | GetDAdminFn
--------------------------------------------------------------------------------


type GetDAdminFn = Tagged (SProxy "getDAdmin(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDAdmin :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
getDAdmin x0 cm r = uncurryFields  r $ getDAdmin' x0 cm
   where
    getDAdmin' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    getDAdmin' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDAdminFn)

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
-- | GetDCategoryFn
--------------------------------------------------------------------------------


type GetDCategoryFn = Tagged (SProxy "getDCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDCategory :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), categoryId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
getDCategory x0 cm r = uncurryFields  r $ getDCategory' x0 cm
   where
    getDCategory' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
    getDCategory' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDCategoryFn)

--------------------------------------------------------------------------------
-- | DAddCategoryFn
--------------------------------------------------------------------------------


type DAddCategoryFn = Tagged (SProxy "dAddCategory(bytes32,bytes32,bool,uint256)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6)))

dAddCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), categoryName :: (BytesN (D3 :& DOne D2)), hasParent :: Boolean, parent :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dAddCategory x0 r = uncurryFields  r $ dAddCategory' x0
   where
    dAddCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryName") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hasParent") Boolean -> Tagged (SProxy "parent") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
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
-- | GetDErc20Fn
--------------------------------------------------------------------------------


type GetDErc20Fn = Tagged (SProxy "getDErc20(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDErc20 :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
getDErc20 x0 cm r = uncurryFields  r $ getDErc20' x0 cm
   where
    getDErc20' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    getDErc20' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDErc20Fn)

--------------------------------------------------------------------------------
-- | GetDHashFn
--------------------------------------------------------------------------------


type GetDHashFn = Tagged (SProxy "getDHash(bytes13)") (Tuple1 (BytesN (D1 :& DOne D3)))

getDHash :: forall e. TransactionOptions NoPay -> ChainCursor -> { prefix :: (BytesN (D1 :& DOne D3)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getDHash x0 cm r = uncurryFields  r $ getDHash' x0 cm
   where
    getDHash' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "prefix") (BytesN (D1 :& DOne D3)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getDHash' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDHashFn)

--------------------------------------------------------------------------------
-- | GetDBallotsNFn
--------------------------------------------------------------------------------


type GetDBallotsNFn = Tagged (SProxy "getDBallotsN(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDBallotsN :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDBallotsN x0 cm r = uncurryFields  r $ getDBallotsN' x0 cm
   where
    getDBallotsN' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDBallotsN' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDBallotsNFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | GetPaymentEnabledFn
--------------------------------------------------------------------------------


type GetPaymentEnabledFn = Tagged (SProxy "getPaymentEnabled()") (Tuple0 )

getPaymentEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
getPaymentEnabled x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetPaymentEnabledFn)

--------------------------------------------------------------------------------
-- | DAddBallotFn
--------------------------------------------------------------------------------


type DAddBallotFn = Tagged (SProxy "dAddBallot(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address)

dAddBallot :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), bb :: Address } -> Web3 e HexString
dAddBallot x0 r = uncurryFields  r $ dAddBallot' x0
   where
    dAddBallot' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "bb") Address -> Web3 e HexString
    dAddBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DAddBallotFn)

--------------------------------------------------------------------------------
-- | PaymentsFn
--------------------------------------------------------------------------------


type PaymentsFn = Tagged (SProxy "payments()") (Tuple0 )

payments :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
payments x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PaymentsFn)

--------------------------------------------------------------------------------
-- | GetUpgradePointerFn
--------------------------------------------------------------------------------


type GetUpgradePointerFn = Tagged (SProxy "getUpgradePointer()") (Tuple0 )

getUpgradePointer :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
getUpgradePointer x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetUpgradePointerFn)

--------------------------------------------------------------------------------
-- | BbFactoryFn
--------------------------------------------------------------------------------


type BbFactoryFn = Tagged (SProxy "bbFactory()") (Tuple0 )

bbFactory :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
bbFactory x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BbFactoryFn)

--------------------------------------------------------------------------------
-- | GetDBallotFn
--------------------------------------------------------------------------------


type GetDBallotFn = Tagged (SProxy "getDBallot(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDBallot :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4))))
getDBallot x0 cm r = uncurryFields  r $ getDBallot' x0 cm
   where
    getDBallot' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4))))
    getDBallot' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDBallotFn)

--------------------------------------------------------------------------------
-- | GetGDemocsNFn
--------------------------------------------------------------------------------


type GetGDemocsNFn = Tagged (SProxy "getGDemocsN()") (Tuple0 )

getGDemocsN :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getGDemocsN x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetGDemocsNFn)

--------------------------------------------------------------------------------
-- | GetGErc20ToDemocsFn
--------------------------------------------------------------------------------


type GetGErc20ToDemocsFn = Tagged (SProxy "getGErc20ToDemocs(address)") (Tuple1 Address)

getGErc20ToDemocs :: forall e. TransactionOptions NoPay -> ChainCursor -> { erc20 :: Address } -> Web3 e (Either CallError (Array (BytesN (D3 :& DOne D2))))
getGErc20ToDemocs x0 cm r = uncurryFields  r $ getGErc20ToDemocs' x0 cm
   where
    getGErc20ToDemocs' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "erc20") Address -> Web3 e (Either CallError (Array (BytesN (D3 :& DOne D2))))
    getGErc20ToDemocs' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetGErc20ToDemocsFn)

--------------------------------------------------------------------------------
-- | SetBackendFn
--------------------------------------------------------------------------------


type SetBackendFn = Tagged (SProxy "setBackend(address)") (Tuple1 Address)

setBackend :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
setBackend x0 r = uncurryFields  r $ setBackend' x0
   where
    setBackend' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    setBackend' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetBackendFn)

--------------------------------------------------------------------------------
-- | GetDInfoFn
--------------------------------------------------------------------------------


type GetDInfoFn = Tagged (SProxy "getDInfo(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDInfo :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple3 Address Address (UIntN (D2 :& D5 :& DOne D6))))
getDInfo x0 cm r = uncurryFields  r $ getDInfo' x0 cm
   where
    getDInfo' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple3 Address Address (UIntN (D2 :& D5 :& DOne D6))))
    getDInfo' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDInfoFn)

--------------------------------------------------------------------------------
-- | SetDAdminFn
--------------------------------------------------------------------------------


type SetDAdminFn = Tagged (SProxy "setDAdmin(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDAdmin :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newAdmin :: Address } -> Web3 e HexString
setDAdmin x0 r = uncurryFields  r $ setDAdmin' x0
   where
    setDAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    setDAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDAdminFn)

--------------------------------------------------------------------------------
-- | GetDBallotAddrFn
--------------------------------------------------------------------------------


type GetDBallotAddrFn = Tagged (SProxy "getDBallotAddr(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDBallotAddr :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getDBallotAddr x0 cm r = uncurryFields  r $ getDBallotAddr' x0 cm
   where
    getDBallotAddr' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getDBallotAddr' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDBallotAddrFn)

--------------------------------------------------------------------------------
-- | EnsPxFn
--------------------------------------------------------------------------------


type EnsPxFn = Tagged (SProxy "ensPx()") (Tuple0 )

ensPx :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
ensPx x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: EnsPxFn)

--------------------------------------------------------------------------------
-- | GetPayToFn
--------------------------------------------------------------------------------


type GetPayToFn = Tagged (SProxy "getPayTo()") (Tuple0 )

getPayTo :: forall e. TransactionOptions NoPay -> Web3 e HexString
getPayTo x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetPayToFn)

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
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address,address,address,address)") (Tuple5 Address Address Address Address Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _b :: Address, _pay :: Address, _pxF :: Address, _bbF :: Address, _ensPx :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_b") Address -> Tagged (SProxy "_pay") Address -> Tagged (SProxy "_pxF") Address -> Tagged (SProxy "_bbF") Address -> Tagged (SProxy "_ensPx") Address -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 y5 y6 = deployContract y0 bc' ((tagged $ Tuple5 (untagged y2 ) (untagged y3 ) (untagged y4 ) (untagged y5 ) (untagged y6 )) :: ConstructorFn)

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
-- | DemocAdded
--------------------------------------------------------------------------------


newtype DemocAdded = DemocAdded {democHash :: (BytesN (D3 :& DOne D2)),admin :: Address}

derive instance newtypeDemocAdded :: Newtype DemocAdded _

instance eventFilterDemocAdded :: EventFilter DemocAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a3d25223b03bc5efb59ee8ede174c158d0a82a774249ed58a8cd3d4d818bd74a")]

instance indexedEventDemocAdded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "admin") Address)) DemocAdded where
  isAnonymous _ = false

derive instance genericDemocAdded :: Generic DemocAdded _

instance eventGenericDemocAddedShow :: Show DemocAdded where
	show = genericShow

instance eventGenericDemocAddedeq :: Eq DemocAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BallotAdded
--------------------------------------------------------------------------------


newtype BallotAdded = BallotAdded {democHash :: (BytesN (D3 :& DOne D2)),id :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBallotAdded :: Newtype BallotAdded _

instance eventFilterBallotAdded :: EventFilter BallotAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "dc273ca230283938fefd191bc6bf22e7d6e445e3a6619722c57f72b69389776a")]

instance indexedEventBallotAdded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)))) BallotAdded where
  isAnonymous _ = false

derive instance genericBallotAdded :: Generic BallotAdded _

instance eventGenericBallotAddedShow :: Show BallotAdded where
	show = genericShow

instance eventGenericBallotAddedeq :: Eq BallotAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PaymentTooLow
--------------------------------------------------------------------------------


newtype PaymentTooLow = PaymentTooLow {msgValue :: (UIntN (D2 :& D5 :& DOne D6)),feeReq :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePaymentTooLow :: Newtype PaymentTooLow _

instance eventFilterPaymentTooLow :: EventFilter PaymentTooLow where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0cbe2bf2897282a407ced6374d0e65b5da5eeef9c81b0b809bc3dda776302142")]

instance indexedEventPaymentTooLow :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "msgValue") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "feeReq") (UIntN (D2 :& D5 :& DOne D6)))) PaymentTooLow where
  isAnonymous _ = false

derive instance genericPaymentTooLow :: Generic PaymentTooLow _

instance eventGenericPaymentTooLowShow :: Show PaymentTooLow where
	show = genericShow

instance eventGenericPaymentTooLoweq :: Eq PaymentTooLow where
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
-- | Error
--------------------------------------------------------------------------------


newtype Error = Error {code :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeError :: Newtype Error _

instance eventFilterError :: EventFilter Error where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2e36a7093f25f22bd4cbdeb6040174c3ba4c5fe8f1abc04e7c3c48f26c7413e0")]

instance indexedEventError :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "code") (UIntN (D2 :& D5 :& DOne D6)))) Error where
  isAnonymous _ = false

derive instance genericError :: Generic Error _

instance eventGenericErrorShow :: Show Error where
	show = genericShow

instance eventGenericErroreq :: Eq Error where
	eq = genericEq