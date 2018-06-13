--------------------------------------------------------------------------------
-- | IxIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxIface where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5, UIntN, unTuple1)
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

getDBallotBox :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getDBallotBox x0 cm r = uncurryFields  r $ getDBallotBox' x0 cm
   where
    getDBallotBox' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
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
-- | GetVersionFn
--------------------------------------------------------------------------------


type GetVersionFn = Tagged (SProxy "getVersion()") (Tuple0 )

getVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetVersionFn)

--------------------------------------------------------------------------------
-- | DInitFn
--------------------------------------------------------------------------------


type DInitFn = Tagged (SProxy "dInit(address)") (Tuple1 Address)

dInit :: forall e. TransactionOptions Wei -> { defualtErc20 :: Address } -> Web3 e HexString
dInit x0 r = uncurryFields  r $ dInit' x0
   where
    dInit' :: TransactionOptions Wei -> Tagged (SProxy "defualtErc20") Address -> Web3 e HexString
    dInit' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DInitFn)

--------------------------------------------------------------------------------
-- | GetCommunityBallotCentsPriceFn
--------------------------------------------------------------------------------


type GetCommunityBallotCentsPriceFn = Tagged (SProxy "getCommunityBallotCentsPrice()") (Tuple0 )

getCommunityBallotCentsPrice :: forall e. TransactionOptions NoPay -> Web3 e HexString
getCommunityBallotCentsPrice x0 = sendTx x0 ((tagged $ Tuple0 ) :: GetCommunityBallotCentsPriceFn)

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

setPaymentBackend :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
setPaymentBackend x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetPaymentBackendFn)

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

doUpgrade :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
doUpgrade x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: DoUpgradeFn)

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

setBackend :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
setBackend x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetBackendFn)

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