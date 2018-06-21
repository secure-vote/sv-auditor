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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D1, D2, D3, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
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
-- | DAddFn
--------------------------------------------------------------------------------


type DAddFn = Tagged (SProxy "dAdd(bytes32,address,bool)") (Tuple3 (BytesN (D3 :& DOne D2)) Address Boolean)

dAdd :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), erc20 :: Address, disableErc20OwnerClaim :: Boolean } -> Web3 e HexString
dAdd x0 r = uncurryFields  r $ dAdd' x0
   where
    dAdd' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "erc20") Address -> Tagged (SProxy "disableErc20OwnerClaim") Boolean -> Web3 e HexString
    dAdd' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DAddFn)

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
-- | GetGDemocFn
--------------------------------------------------------------------------------


type GetGDemocFn = Tagged (SProxy "getGDemoc(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getGDemoc :: forall e. TransactionOptions NoPay -> ChainCursor -> { id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getGDemoc x0 cm r = uncurryFields  r $ getGDemoc' x0 cm
   where
    getGDemoc' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getGDemoc' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetGDemocFn)

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
-- | GetAdminLogFn
--------------------------------------------------------------------------------


type GetAdminLogFn = Tagged (SProxy "getAdminLog(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getAdminLog :: forall e. TransactionOptions NoPay -> ChainCursor -> { n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError Address)
getAdminLog x0 cm r = uncurryFields  r $ getAdminLog' x0 cm
   where
    getAdminLog' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Address)
    getAdminLog' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetAdminLogFn)

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
-- | DSetCommunityBallotsEnabledFn
--------------------------------------------------------------------------------


type DSetCommunityBallotsEnabledFn = Tagged (SProxy "dSetCommunityBallotsEnabled(bytes32,bool)") (Tuple2 (BytesN (D3 :& DOne D2)) Boolean)

dSetCommunityBallotsEnabled :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), enabled :: Boolean } -> Web3 e HexString
dSetCommunityBallotsEnabled x0 r = uncurryFields  r $ dSetCommunityBallotsEnabled' x0
   where
    dSetCommunityBallotsEnabled' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "enabled") Boolean -> Web3 e HexString
    dSetCommunityBallotsEnabled' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: DSetCommunityBallotsEnabledFn)

--------------------------------------------------------------------------------
-- | GetDCategoryFn
--------------------------------------------------------------------------------


type GetDCategoryFn = Tagged (SProxy "getDCategory(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDCategory :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), catId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
getDCategory x0 cm r = uncurryFields  r $ getDCategory' x0 cm
   where
    getDCategory' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "catId") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 Boolean (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6))))
    getDCategory' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDCategoryFn)

--------------------------------------------------------------------------------
-- | DAddCategoryFn
--------------------------------------------------------------------------------


type DAddCategoryFn = Tagged (SProxy "dAddCategory(bytes32,bytes32,bool,uint256)") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Boolean (UIntN (D2 :& D5 :& DOne D6)))

dAddCategory :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), name :: (BytesN (D3 :& DOne D2)), hasParent :: Boolean, parent :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
dAddCategory x0 r = uncurryFields  r $ dAddCategory' x0
   where
    dAddCategory' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "name") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "hasParent") Boolean -> Tagged (SProxy "parent") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e HexString
    dAddCategory' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: DAddCategoryFn)

--------------------------------------------------------------------------------
-- | CurrAdminEpochFn
--------------------------------------------------------------------------------


type CurrAdminEpochFn = Tagged (SProxy "currAdminEpoch()") (Tuple0 )

currAdminEpoch :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
currAdminEpoch x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CurrAdminEpochFn)

--------------------------------------------------------------------------------
-- | GetAdminLogNFn
--------------------------------------------------------------------------------


type GetAdminLogNFn = Tagged (SProxy "getAdminLogN()") (Tuple0 )

getAdminLogN :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getAdminLogN x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetAdminLogNFn)

--------------------------------------------------------------------------------
-- | IncAdminEpochFn
--------------------------------------------------------------------------------


type IncAdminEpochFn = Tagged (SProxy "incAdminEpoch()") (Tuple0 )

incAdminEpoch :: forall e. TransactionOptions NoPay -> Web3 e HexString
incAdminEpoch x0 = sendTx x0 ((tagged $ Tuple0 ) :: IncAdminEpochFn)

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
-- | SetPermissionsFn
--------------------------------------------------------------------------------


type SetPermissionsFn = Tagged (SProxy "setPermissions(address,bool)") (Tuple2 Address Boolean)

setPermissions :: forall e. TransactionOptions NoPay -> { e :: Address, _editPerms :: Boolean } -> Web3 e HexString
setPermissions x0 r = uncurryFields  r $ setPermissions' x0
   where
    setPermissions' :: TransactionOptions NoPay -> Tagged (SProxy "e") Address -> Tagged (SProxy "_editPerms") Boolean -> Web3 e HexString
    setPermissions' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetPermissionsFn)

--------------------------------------------------------------------------------
-- | GetDCountedBasicBallotIDFn
--------------------------------------------------------------------------------


type GetDCountedBasicBallotIDFn = Tagged (SProxy "getDCountedBasicBallotID(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDCountedBasicBallotID :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDCountedBasicBallotID x0 cm r = uncurryFields  r $ getDCountedBasicBallotID' x0 cm
   where
    getDCountedBasicBallotID' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDCountedBasicBallotID' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDCountedBasicBallotIDFn)

--------------------------------------------------------------------------------
-- | GetDCommBallotsEnabledFn
--------------------------------------------------------------------------------


type GetDCommBallotsEnabledFn = Tagged (SProxy "getDCommBallotsEnabled(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDCommBallotsEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
getDCommBallotsEnabled x0 cm r = uncurryFields  r $ getDCommBallotsEnabled' x0 cm
   where
    getDCommBallotsEnabled' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    getDCommBallotsEnabled' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDCommBallotsEnabledFn)

--------------------------------------------------------------------------------
-- | GetDOwnerFn
--------------------------------------------------------------------------------


type GetDOwnerFn = Tagged (SProxy "getDOwner(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDOwner :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
getDOwner x0 cm r = uncurryFields  r $ getDOwner' x0 cm
   where
    getDOwner' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    getDOwner' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDOwnerFn)

--------------------------------------------------------------------------------
-- | GetDEditorArbitraryDataFn
--------------------------------------------------------------------------------


type GetDEditorArbitraryDataFn = Tagged (SProxy "getDEditorArbitraryData(bytes32,bytes)") (Tuple2 (BytesN (D3 :& DOne D2)) ByteString)

getDEditorArbitraryData :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), key :: ByteString } -> Web3 e (Either CallError ByteString)
getDEditorArbitraryData x0 cm r = uncurryFields  r $ getDEditorArbitraryData' x0 cm
   where
    getDEditorArbitraryData' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") ByteString -> Web3 e (Either CallError ByteString)
    getDEditorArbitraryData' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDEditorArbitraryDataFn)

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
-- | AdminLockdownFn
--------------------------------------------------------------------------------


type AdminLockdownFn = Tagged (SProxy "adminLockdown()") (Tuple0 )

adminLockdown :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminLockdown x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminLockdownFn)

--------------------------------------------------------------------------------
-- | UpgradeMeAdminFn
--------------------------------------------------------------------------------


type UpgradeMeAdminFn = Tagged (SProxy "upgradeMeAdmin(address)") (Tuple1 Address)

upgradeMeAdmin :: forall e. TransactionOptions NoPay -> { newAdmin :: Address } -> Web3 e HexString
upgradeMeAdmin x0 r = uncurryFields  r $ upgradeMeAdmin' x0
   where
    upgradeMeAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    upgradeMeAdmin' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: UpgradeMeAdminFn)

--------------------------------------------------------------------------------
-- | GetDErc20OwnerClaimEnabledFn
--------------------------------------------------------------------------------


type GetDErc20OwnerClaimEnabledFn = Tagged (SProxy "getDErc20OwnerClaimEnabled(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDErc20OwnerClaimEnabled :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Boolean)
getDErc20OwnerClaimEnabled x0 cm r = uncurryFields  r $ getDErc20OwnerClaimEnabled' x0 cm
   where
    getDErc20OwnerClaimEnabled' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Boolean)
    getDErc20OwnerClaimEnabled' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDErc20OwnerClaimEnabledFn)

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
-- | DSetArbitraryDataFn
--------------------------------------------------------------------------------


type DSetArbitraryDataFn = Tagged (SProxy "dSetArbitraryData(bytes32,bytes,bytes)") (Tuple3 (BytesN (D3 :& DOne D2)) ByteString ByteString)

dSetArbitraryData :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), key :: ByteString, value :: ByteString } -> Web3 e HexString
dSetArbitraryData x0 r = uncurryFields  r $ dSetArbitraryData' x0
   where
    dSetArbitraryData' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") ByteString -> Tagged (SProxy "value") ByteString -> Web3 e HexString
    dSetArbitraryData' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DSetArbitraryDataFn)

--------------------------------------------------------------------------------
-- | DAddBallotFn
--------------------------------------------------------------------------------


type DAddBallotFn = Tagged (SProxy "dAddBallot(bytes32,uint256,uint256,bool)") (Tuple4 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Boolean)

dAddBallot :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), ballotId :: (UIntN (D2 :& D5 :& DOne D6)), packed :: (UIntN (D2 :& D5 :& DOne D6)), countTowardsLimit :: Boolean } -> Web3 e HexString
dAddBallot x0 r = uncurryFields  r $ dAddBallot' x0
   where
    dAddBallot' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "countTowardsLimit") Boolean -> Web3 e HexString
    dAddBallot' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: DAddBallotFn)

--------------------------------------------------------------------------------
-- | DSetEditorArbitraryDataFn
--------------------------------------------------------------------------------


type DSetEditorArbitraryDataFn = Tagged (SProxy "dSetEditorArbitraryData(bytes32,bytes,bytes)") (Tuple3 (BytesN (D3 :& DOne D2)) ByteString ByteString)

dSetEditorArbitraryData :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), key :: ByteString, value :: ByteString } -> Web3 e HexString
dSetEditorArbitraryData x0 r = uncurryFields  r $ dSetEditorArbitraryData' x0
   where
    dSetEditorArbitraryData' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") ByteString -> Tagged (SProxy "value") ByteString -> Web3 e HexString
    dSetEditorArbitraryData' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DSetEditorArbitraryDataFn)

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
-- | DInitFn
--------------------------------------------------------------------------------


type DInitFn = Tagged (SProxy "dInit(address,address,bool)") (Tuple3 Address Address Boolean)

dInit :: forall e. TransactionOptions NoPay -> { defaultErc20 :: Address, initOwner :: Address, disableErc20OwnerClaim :: Boolean } -> Web3 e HexString
dInit x0 r = uncurryFields  r $ dInit' x0
   where
    dInit' :: TransactionOptions NoPay -> Tagged (SProxy "defaultErc20") Address -> Tagged (SProxy "initOwner") Address -> Tagged (SProxy "disableErc20OwnerClaim") Boolean -> Web3 e HexString
    dInit' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: DInitFn)

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
-- | SetDOwnerFn
--------------------------------------------------------------------------------


type SetDOwnerFn = Tagged (SProxy "setDOwner(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDOwner :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newOwner :: Address } -> Web3 e HexString
setDOwner x0 r = uncurryFields  r $ setDOwner' x0
   where
    setDOwner' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setDOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDOwnerFn)

--------------------------------------------------------------------------------
-- | EmergencySetDOwnerFn
--------------------------------------------------------------------------------


type EmergencySetDOwnerFn = Tagged (SProxy "emergencySetDOwner(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

emergencySetDOwner :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newOwner :: Address } -> Web3 e HexString
emergencySetDOwner x0 r = uncurryFields  r $ emergencySetDOwner' x0
   where
    emergencySetDOwner' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    emergencySetDOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: EmergencySetDOwnerFn)

--------------------------------------------------------------------------------
-- | GetDBallotIDFn
--------------------------------------------------------------------------------


type GetDBallotIDFn = Tagged (SProxy "getDBallotID(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

getDBallotID :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), n :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDBallotID x0 cm r = uncurryFields  r $ getDBallotID' x0 cm
   where
    getDBallotID' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "n") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDBallotID' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDBallotIDFn)

--------------------------------------------------------------------------------
-- | IsDEditorFn
--------------------------------------------------------------------------------


type IsDEditorFn = Tagged (SProxy "isDEditor(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

isDEditor :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), editor :: Address } -> Web3 e (Either CallError Boolean)
isDEditor x0 cm r = uncurryFields  r $ isDEditor' x0 cm
   where
    isDEditor' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "editor") Address -> Web3 e (Either CallError Boolean)
    isDEditor' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: IsDEditorFn)

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
-- | GetDCountedBasicBallotsNFn
--------------------------------------------------------------------------------


type GetDCountedBasicBallotsNFn = Tagged (SProxy "getDCountedBasicBallotsN(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDCountedBasicBallotsN :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getDCountedBasicBallotsN x0 cm r = uncurryFields  r $ getDCountedBasicBallotsN' x0 cm
   where
    getDCountedBasicBallotsN' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getDCountedBasicBallotsN' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDCountedBasicBallotsNFn)

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
-- | GetDArbitraryDataFn
--------------------------------------------------------------------------------


type GetDArbitraryDataFn = Tagged (SProxy "getDArbitraryData(bytes32,bytes)") (Tuple2 (BytesN (D3 :& DOne D2)) ByteString)

getDArbitraryData :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)), key :: ByteString } -> Web3 e (Either CallError ByteString)
getDArbitraryData x0 cm r = uncurryFields  r $ getDArbitraryData' x0 cm
   where
    getDArbitraryData' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "key") ByteString -> Web3 e (Either CallError ByteString)
    getDArbitraryData' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: GetDArbitraryDataFn)

--------------------------------------------------------------------------------
-- | SetDOwnerFromClaimFn
--------------------------------------------------------------------------------


type SetDOwnerFromClaimFn = Tagged (SProxy "setDOwnerFromClaim(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDOwnerFromClaim :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newOwner :: Address } -> Web3 e HexString
setDOwnerFromClaim x0 r = uncurryFields  r $ setDOwnerFromClaim' x0
   where
    setDOwnerFromClaim' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newOwner") Address -> Web3 e HexString
    setDOwnerFromClaim' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDOwnerFromClaimFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: forall e. TransactionOptions NoPay -> HexString -> Web3 e HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

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