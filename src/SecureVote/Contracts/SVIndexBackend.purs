--------------------------------------------------------------------------------
-- | SVIndexBackend
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVIndexBackend where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | AddCategoryFn
--------------------------------------------------------------------------------


type AddCategoryFn = Tagged (SProxy "addCategory(bytes32,bytes32,bool,uint256)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6)))

addCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), categoryName :: (BytesN (D3 :& DOne D2)), hasParent :: Boolean, parent :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
addCategory x0 r = uncurryFields  r $ addCategory' x0
   where
    addCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryName") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hasParent") Boolean -> Tagged (SProxy "parent") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    addCategory' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: AddCategoryFn)

--------------------------------------------------------------------------------
-- | GetBallotBoxFn
--------------------------------------------------------------------------------


type GetBallotBoxFn = Tagged (SProxy "getBallotBox(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getBallotBox :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getBallotBox x0 cm r = uncurryFields  r $ getBallotBox' x0 cm
   where
    getBallotBox' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getBallotBox' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetBallotBoxFn)

--------------------------------------------------------------------------------
-- | DoLockdownFn
--------------------------------------------------------------------------------


type DoLockdownFn = Tagged (SProxy "doLockdown()") (Tuple0 )

doLockdown :: forall e. TransactionOptions NoPay -> Web3 e HexString
doLockdown x0 = sendTx x0 ((tagged $ Tuple0 ) :: DoLockdownFn)

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
-- | HasPermissionsFn
--------------------------------------------------------------------------------


type HasPermissionsFn = Tagged (SProxy "hasPermissions(address)") (Tuple1 Address)

hasPermissions :: forall e. TransactionOptions NoPay -> ChainCursor -> { a :: Address } -> Web3 e (Either CallError Boolean)
hasPermissions x0 cm r = uncurryFields  r $ hasPermissions' x0 cm
   where
    hasPermissions' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "a") Address -> Web3 e (Either CallError Boolean)
    hasPermissions' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: HasPermissionsFn)

--------------------------------------------------------------------------------
-- | IsAdminFn
--------------------------------------------------------------------------------


type IsAdminFn = Tagged (SProxy "isAdmin(address)") (Tuple1 Address)

isAdmin :: forall e. TransactionOptions NoPay -> ChainCursor -> { a :: Address } -> Web3 e (Either CallError Boolean)
isAdmin x0 cm r = uncurryFields  r $ isAdmin' x0 cm
   where
    isAdmin' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "a") Address -> Web3 e (Either CallError Boolean)
    isAdmin' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: IsAdminFn)

--------------------------------------------------------------------------------
-- | GetDemocInfoFn
--------------------------------------------------------------------------------


type GetDemocInfoFn = Tagged (SProxy "getDemocInfo(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDemocInfo :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& DOne D6))))
getDemocInfo x0 cm r = uncurryFields  r $ getDemocInfo' x0 cm
   where
    getDemocInfo' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple3 String Address (UIntN (D2 :& D5 :& DOne D6))))
    getDemocInfo' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocInfoFn)

--------------------------------------------------------------------------------
-- | GetDNameFn
--------------------------------------------------------------------------------


type GetDNameFn = Tagged (SProxy "getDName(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDName :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError String)
getDName x0 cm r = uncurryFields  r $ getDName' x0 cm
   where
    getDName' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError String)
    getDName' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDNameFn)

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
-- | CurrAdminEpochFn
--------------------------------------------------------------------------------


type CurrAdminEpochFn = Tagged (SProxy "currAdminEpoch()") (Tuple0 )

currAdminEpoch :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
currAdminEpoch x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CurrAdminEpochFn)

--------------------------------------------------------------------------------
-- | BallotListFn
--------------------------------------------------------------------------------


type BallotListFn = Tagged (SProxy "ballotList(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

ballotList :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
ballotList x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: BallotListFn)

--------------------------------------------------------------------------------
-- | IncAdminEpochFn
--------------------------------------------------------------------------------


type IncAdminEpochFn = Tagged (SProxy "incAdminEpoch()") (Tuple0 )

incAdminEpoch :: forall e. TransactionOptions NoPay -> Web3 e HexString
incAdminEpoch x0 = sendTx x0 ((tagged $ Tuple0 ) :: IncAdminEpochFn)

--------------------------------------------------------------------------------
-- | GetDemocHashFn
--------------------------------------------------------------------------------


type GetDemocHashFn = Tagged (SProxy "getDemocHash(bytes13)") (Tuple1 (BytesN (D1 :& DOne D3)))

getDemocHash :: forall e. TransactionOptions NoPay -> ChainCursor -> { prefix :: (BytesN (D1 :& DOne D3)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getDemocHash x0 cm r = uncurryFields  r $ getDemocHash' x0 cm
   where
    getDemocHash' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "prefix") (BytesN (D1 :& DOne D3)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getDemocHash' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocHashFn)

--------------------------------------------------------------------------------
-- | SetAdminFn
--------------------------------------------------------------------------------


type SetAdminFn = Tagged (SProxy "setAdmin(address,bool)") (Tuple2 Address Boolean)

setAdmin :: forall e. TransactionOptions NoPay -> { a :: Address, _givePerms :: Boolean } -> Web3 e HexString
setAdmin x0 r = uncurryFields  r $ setAdmin' x0
   where
    setAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "a") Address -> Tagged (SProxy "_givePerms") Boolean -> Web3 e HexString
    setAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetAdminFn)

--------------------------------------------------------------------------------
-- | UpgradeMeFn
--------------------------------------------------------------------------------


type UpgradeMeFn = Tagged (SProxy "upgradeMe(address)") (Tuple1 Address)

upgradeMe :: forall e. TransactionOptions NoPay -> { newSC :: Address } -> Web3 e HexString
upgradeMe x0 r = uncurryFields  r $ upgradeMe' x0
   where
    upgradeMe' :: TransactionOptions NoPay -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    upgradeMe' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeMeFn)

--------------------------------------------------------------------------------
-- | AdminsDisabledForeverFn
--------------------------------------------------------------------------------


type AdminsDisabledForeverFn = Tagged (SProxy "adminsDisabledForever()") (Tuple0 )

adminsDisabledForever :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminsDisabledForever x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminsDisabledForeverFn)

--------------------------------------------------------------------------------
-- | GetDemocCategoryFn
--------------------------------------------------------------------------------


type GetDemocCategoryFn = Tagged (SProxy "getDemocCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDemocCategory :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), categoryId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
getDemocCategory x0 cm r = uncurryFields  r $ getDemocCategory' x0 cm
   where
    getDemocCategory' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
    getDemocCategory' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDemocCategoryFn)

--------------------------------------------------------------------------------
-- | NDemocsFn
--------------------------------------------------------------------------------


type NDemocsFn = Tagged (SProxy "nDemocs()") (Tuple0 )

nDemocs :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nDemocs x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NDemocsFn)

--------------------------------------------------------------------------------
-- | SetPermissionsFn
--------------------------------------------------------------------------------


type SetPermissionsFn = Tagged (SProxy "setPermissions(address,bool)") (Tuple2 Address Boolean)

setPermissions :: forall e. TransactionOptions NoPay -> { e :: Address, _editPerms :: Boolean } -> Web3 e HexString
setPermissions x0 r = uncurryFields  r $ setPermissions' x0
   where
    setPermissions' :: TransactionOptions NoPay -> Tagged (SProxy "e") Address -> Tagged (SProxy "_editPerms") Boolean -> Web3 e HexString
    setPermissions' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetPermissionsFn)

--------------------------------------------------------------------------------
-- | AdminLockdownFn
--------------------------------------------------------------------------------


type AdminLockdownFn = Tagged (SProxy "adminLockdown()") (Tuple0 )

adminLockdown :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminLockdown x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminLockdownFn)

--------------------------------------------------------------------------------
-- | NBallotsGlobalFn
--------------------------------------------------------------------------------


type NBallotsGlobalFn = Tagged (SProxy "nBallotsGlobal()") (Tuple0 )

nBallotsGlobal :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nBallotsGlobal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NBallotsGlobalFn)

--------------------------------------------------------------------------------
-- | DemocPrefixToHashFn
--------------------------------------------------------------------------------


type DemocPrefixToHashFn = Tagged (SProxy "democPrefixToHash(bytes13)") (Tuple1 (BytesN (D1 :& DOne D3)))

democPrefixToHash :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN (D1 :& DOne D3)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
democPrefixToHash x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DemocPrefixToHashFn)

--------------------------------------------------------------------------------
-- | GetNthBallotFn
--------------------------------------------------------------------------------


type GetNthBallotFn = Tagged (SProxy "getNthBallot(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getNthBallot :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4))))
getNthBallot x0 cm r = uncurryFields  r $ getNthBallot' x0 cm
   where
    getNthBallot' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4))))
    getNthBallot' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetNthBallotFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | DemocsFn
--------------------------------------------------------------------------------


type DemocsFn = Tagged (SProxy "democs(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

democs :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple2 String Address))
democs x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: DemocsFn)

--------------------------------------------------------------------------------
-- | AddBallotFn
--------------------------------------------------------------------------------


type AddBallotFn = Tagged (SProxy "addBallot(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address)

addBallot :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), bb :: Address } -> Web3 e HexString
addBallot x0 r = uncurryFields  r $ addBallot' x0
   where
    addBallot' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "bb") Address -> Web3 e HexString
    addBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: AddBallotFn)

--------------------------------------------------------------------------------
-- | UpgradePermissionedSCFn
--------------------------------------------------------------------------------


type UpgradePermissionedSCFn = Tagged (SProxy "upgradePermissionedSC(address,address)") (Tuple2 Address Address)

upgradePermissionedSC :: forall e. TransactionOptions NoPay -> { oldSC :: Address, newSC :: Address } -> Web3 e HexString
upgradePermissionedSC x0 r = uncurryFields  r $ upgradePermissionedSC' x0
   where
    upgradePermissionedSC' :: TransactionOptions NoPay -> Tagged (SProxy "oldSC") Address -> Tagged (SProxy "newSC") Address -> Web3 e HexString
    upgradePermissionedSC' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: UpgradePermissionedSCFn)

--------------------------------------------------------------------------------
-- | GetBallotAddrFn
--------------------------------------------------------------------------------


type GetBallotAddrFn = Tagged (SProxy "getBallotAddr(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getBallotAddr :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getBallotAddr x0 cm r = uncurryFields  r $ getBallotAddr' x0 cm
   where
    getBallotAddr' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getBallotAddr' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetBallotAddrFn)

--------------------------------------------------------------------------------
-- | GetDemocNCategoriesFn
--------------------------------------------------------------------------------


type GetDemocNCategoriesFn = Tagged (SProxy "getDemocNCategories(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDemocNCategories :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDemocNCategories x0 cm r = uncurryFields  r $ getDemocNCategories' x0 cm
   where
    getDemocNCategories' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDemocNCategories' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocNCategoriesFn)

--------------------------------------------------------------------------------
-- | NBallotsFn
--------------------------------------------------------------------------------


type NBallotsFn = Tagged (SProxy "nBallots(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

nBallots :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nBallots x0 cm r = uncurryFields  r $ nBallots' x0 cm
   where
    nBallots' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    nBallots' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: NBallotsFn)

--------------------------------------------------------------------------------
-- | DemocListFn
--------------------------------------------------------------------------------


type DemocListFn = Tagged (SProxy "democList(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

democList :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
democList x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DemocListFn)

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
-- | DemocCategoriesFn
--------------------------------------------------------------------------------


type DemocCategoriesFn = Tagged (SProxy "democCategories(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

democCategories :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
democCategories x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DemocCategoriesFn)

--------------------------------------------------------------------------------
-- | DeprecateCategoryFn
--------------------------------------------------------------------------------


type DeprecateCategoryFn = Tagged (SProxy "deprecateCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

deprecateCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), categoryId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
deprecateCategory x0 r = uncurryFields  r $ deprecateCategory' x0
   where
    deprecateCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "categoryId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    deprecateCategory' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DeprecateCategoryFn)

--------------------------------------------------------------------------------
-- | InitDemocFn
--------------------------------------------------------------------------------


type InitDemocFn = Tagged (SProxy "initDemoc(string)") (Tuple1 String)

initDemoc :: forall e. TransactionOptions NoPay -> { democName :: String } -> Web3 e HexString
initDemoc x0 r = uncurryFields  r $ initDemoc' x0
   where
    initDemoc' :: TransactionOptions NoPay -> Tagged (SProxy "democName") String -> Web3 e HexString
    initDemoc' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: InitDemocFn)

--------------------------------------------------------------------------------
-- | LowLevelNewBallot
--------------------------------------------------------------------------------


newtype LowLevelNewBallot = LowLevelNewBallot {democHash :: (BytesN (D3 :& DOne D2)),id :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeLowLevelNewBallot :: Newtype LowLevelNewBallot _

instance eventFilterLowLevelNewBallot :: EventFilter LowLevelNewBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f6f57c07a30141547a067c1e27493aa7c387a4b8a37be5a663e9045b7cd7cc3b")]

instance indexedEventLowLevelNewBallot :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)))) LowLevelNewBallot where
  isAnonymous _ = false

derive instance genericLowLevelNewBallot :: Generic LowLevelNewBallot _

instance eventGenericLowLevelNewBallotShow :: Show LowLevelNewBallot where
	show = genericShow

instance eventGenericLowLevelNewBalloteq :: Eq LowLevelNewBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | LowLevelNewDemoc
--------------------------------------------------------------------------------


newtype LowLevelNewDemoc = LowLevelNewDemoc {democHash :: (BytesN (D3 :& DOne D2))}

derive instance newtypeLowLevelNewDemoc :: Newtype LowLevelNewDemoc _

instance eventFilterLowLevelNewDemoc :: EventFilter LowLevelNewDemoc where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "05af2d8adc0cbd4286b8b80a28358800f55c9f7e4f752aa1b1ccf6d9482cfa1a")]

instance indexedEventLowLevelNewDemoc :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)))) LowLevelNewDemoc where
  isAnonymous _ = false

derive instance genericLowLevelNewDemoc :: Generic LowLevelNewDemoc _

instance eventGenericLowLevelNewDemocShow :: Show LowLevelNewDemoc where
	show = genericShow

instance eventGenericLowLevelNewDemoceq :: Eq LowLevelNewDemoc where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionError
--------------------------------------------------------------------------------


newtype PermissionError = PermissionError {editAddr :: Address}

derive instance newtypePermissionError :: Newtype PermissionError _

instance eventFilterPermissionError :: EventFilter PermissionError where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ba1558d6c3ad0688fe6d3e0a7ee68da13b944fc53864b461b74c92e4a9654a3e")]

instance indexedEventPermissionError :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "editAddr") Address)) PermissionError where
  isAnonymous _ = false

derive instance genericPermissionError :: Generic PermissionError _

instance eventGenericPermissionErrorShow :: Show PermissionError where
	show = genericShow

instance eventGenericPermissionErroreq :: Eq PermissionError where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionGranted
--------------------------------------------------------------------------------


newtype PermissionGranted = PermissionGranted {editAddr :: Address}

derive instance newtypePermissionGranted :: Newtype PermissionGranted _

instance eventFilterPermissionGranted :: EventFilter PermissionGranted where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c1f0ea3cc21b72d778e7e9d433c419eabb16edce0afe4468769e055b2e6d49c6")]

instance indexedEventPermissionGranted :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "editAddr") Address)) PermissionGranted where
  isAnonymous _ = false

derive instance genericPermissionGranted :: Generic PermissionGranted _

instance eventGenericPermissionGrantedShow :: Show PermissionGranted where
	show = genericShow

instance eventGenericPermissionGrantedeq :: Eq PermissionGranted where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionRevoked
--------------------------------------------------------------------------------


newtype PermissionRevoked = PermissionRevoked {editAddr :: Address}

derive instance newtypePermissionRevoked :: Newtype PermissionRevoked _

instance eventFilterPermissionRevoked :: EventFilter PermissionRevoked where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3541f93cbae8c4be65491b824efe1570976e740b18c6aa441db5291f4de4c921")]

instance indexedEventPermissionRevoked :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "editAddr") Address)) PermissionRevoked where
  isAnonymous _ = false

derive instance genericPermissionRevoked :: Generic PermissionRevoked _

instance eventGenericPermissionRevokedShow :: Show PermissionRevoked where
	show = genericShow

instance eventGenericPermissionRevokedeq :: Eq PermissionRevoked where
	eq = genericEq

--------------------------------------------------------------------------------
-- | PermissionsUpgraded
--------------------------------------------------------------------------------


newtype PermissionsUpgraded = PermissionsUpgraded {oldSC :: Address,newSC :: Address}

derive instance newtypePermissionsUpgraded :: Newtype PermissionsUpgraded _

instance eventFilterPermissionsUpgraded :: EventFilter PermissionsUpgraded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "14e3af41624ed426a3e0e05e698f9abc5f7c5a80bab49a1b6f7ab4e534702b58")]

instance indexedEventPermissionsUpgraded :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "oldSC") Address) (Tagged (SProxy "newSC") Address)) PermissionsUpgraded where
  isAnonymous _ = false

derive instance genericPermissionsUpgraded :: Generic PermissionsUpgraded _

instance eventGenericPermissionsUpgradedShow :: Show PermissionsUpgraded where
	show = genericShow

instance eventGenericPermissionsUpgradedeq :: Eq PermissionsUpgraded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SelfUpgrade
--------------------------------------------------------------------------------


newtype SelfUpgrade = SelfUpgrade {oldSC :: Address,newSC :: Address}

derive instance newtypeSelfUpgrade :: Newtype SelfUpgrade _

instance eventFilterSelfUpgrade :: EventFilter SelfUpgrade where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "4532cbbb9747736f93100911e83c51f9509459a759d4fe4f8a942688cce83c2a")]

instance indexedEventSelfUpgrade :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "oldSC") Address) (Tagged (SProxy "newSC") Address)) SelfUpgrade where
  isAnonymous _ = false

derive instance genericSelfUpgrade :: Generic SelfUpgrade _

instance eventGenericSelfUpgradeShow :: Show SelfUpgrade where
	show = genericShow

instance eventGenericSelfUpgradeeq :: Eq SelfUpgrade where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminLockdown
--------------------------------------------------------------------------------


newtype AdminLockdown = AdminLockdown {}

derive instance newtypeAdminLockdown :: Newtype AdminLockdown _

instance eventFilterAdminLockdown :: EventFilter AdminLockdown where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2fa084a3abd5513daa7f5bfb140cf0ae5d4e4bb7ec06479fe25956313701a205")]

instance indexedEventAdminLockdown :: IndexedEvent (Tuple0 ) (Tuple0 ) AdminLockdown where
  isAnonymous _ = false

derive instance genericAdminLockdown :: Generic AdminLockdown _

instance eventGenericAdminLockdownShow :: Show AdminLockdown where
	show = genericShow

instance eventGenericAdminLockdowneq :: Eq AdminLockdown where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminAdded
--------------------------------------------------------------------------------


newtype AdminAdded = AdminAdded {newAdmin :: Address}

derive instance newtypeAdminAdded :: Newtype AdminAdded _

instance eventFilterAdminAdded :: EventFilter AdminAdded where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "44d6d25963f097ad14f29f06854a01f575648a1ef82f30e562ccd3889717e339"),Nothing]

instance indexedEventAdminAdded :: IndexedEvent (Tuple1 (Tagged (SProxy "newAdmin") Address)) (Tuple0 ) AdminAdded where
  isAnonymous _ = false

derive instance genericAdminAdded :: Generic AdminAdded _

instance eventGenericAdminAddedShow :: Show AdminAdded where
	show = genericShow

instance eventGenericAdminAddedeq :: Eq AdminAdded where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminRemoved
--------------------------------------------------------------------------------


newtype AdminRemoved = AdminRemoved {oldAdmin :: Address}

derive instance newtypeAdminRemoved :: Newtype AdminRemoved _

instance eventFilterAdminRemoved :: EventFilter AdminRemoved where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a3b62bc36326052d97ea62d63c3d60308ed4c3ea8ac079dd8499f1e9c4f80c0f"),Nothing]

instance indexedEventAdminRemoved :: IndexedEvent (Tuple1 (Tagged (SProxy "oldAdmin") Address)) (Tuple0 ) AdminRemoved where
  isAnonymous _ = false

derive instance genericAdminRemoved :: Generic AdminRemoved _

instance eventGenericAdminRemovedShow :: Show AdminRemoved where
	show = genericShow

instance eventGenericAdminRemovedeq :: Eq AdminRemoved where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminEpochInc
--------------------------------------------------------------------------------


newtype AdminEpochInc = AdminEpochInc {}

derive instance newtypeAdminEpochInc :: Newtype AdminEpochInc _

instance eventFilterAdminEpochInc :: EventFilter AdminEpochInc where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c536428a6a2ea6a7cff457a274794564f9f6ce1cfcf4c0a53fadaa231b017d8a")]

instance indexedEventAdminEpochInc :: IndexedEvent (Tuple0 ) (Tuple0 ) AdminEpochInc where
  isAnonymous _ = false

derive instance genericAdminEpochInc :: Generic AdminEpochInc _

instance eventGenericAdminEpochIncShow :: Show AdminEpochInc where
	show = genericShow

instance eventGenericAdminEpochInceq :: Eq AdminEpochInc where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AdminDisabledForever
--------------------------------------------------------------------------------


newtype AdminDisabledForever = AdminDisabledForever {}

derive instance newtypeAdminDisabledForever :: Newtype AdminDisabledForever _

instance eventFilterAdminDisabledForever :: EventFilter AdminDisabledForever where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e6c1892f8d36012439015afa98d305e0aa27017e4042014c39690c8626d4a4a1")]

instance indexedEventAdminDisabledForever :: IndexedEvent (Tuple0 ) (Tuple0 ) AdminDisabledForever where
  isAnonymous _ = false

derive instance genericAdminDisabledForever :: Generic AdminDisabledForever _

instance eventGenericAdminDisabledForeverShow :: Show AdminDisabledForever where
	show = genericShow

instance eventGenericAdminDisabledForevereq :: Eq AdminDisabledForever where
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