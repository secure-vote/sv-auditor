--------------------------------------------------------------------------------
-- | IxBackendIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxBackendIface where

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
-- | DInitFn
--------------------------------------------------------------------------------


type DInitFn = Tagged (SProxy "dInit(address)") (Tuple1 Address)

dInit :: forall e. TransactionOptions NoPay -> { defaultErc20 :: Address } -> Web3 e HexString
dInit x0 r = uncurryFields  r $ dInit' x0
   where
    dInit' :: TransactionOptions NoPay -> Tagged (SProxy "defaultErc20") Address -> Web3 e HexString
    dInit' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: DInitFn)

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
-- | GetDAdminFn
--------------------------------------------------------------------------------


type GetDAdminFn = Tagged (SProxy "getDAdmin(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getDAdmin :: forall e. TransactionOptions NoPay -> ChainCursor -> { democHash :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError Address)
getDAdmin x0 cm r = uncurryFields  r $ getDAdmin' x0 cm
   where
    getDAdmin' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError Address)
    getDAdmin' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDAdminFn)

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
-- | UpgradeMeFn
--------------------------------------------------------------------------------


type UpgradeMeFn = Tagged (SProxy "upgradeMe(address)") (Tuple1 Address)

upgradeMe :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
upgradeMe x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: UpgradeMeFn)

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
-- | GetGBallotsNFn
--------------------------------------------------------------------------------


type GetGBallotsNFn = Tagged (SProxy "getGBallotsN()") (Tuple0 )

getGBallotsN :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getGBallotsN x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetGBallotsNFn)

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
-- | GetGBallotFn
--------------------------------------------------------------------------------


type GetGBallotFn = Tagged (SProxy "getGBallot(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getGBallot :: forall e. TransactionOptions NoPay -> ChainCursor -> { id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
getGBallot x0 cm r = uncurryFields  r $ getGBallot' x0 cm
   where
    getGBallot' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
    getGBallot' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetGBallotFn)

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
-- | SetDErc20Fn
--------------------------------------------------------------------------------


type SetDErc20Fn = Tagged (SProxy "setDErc20(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDErc20 :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newErc20 :: Address } -> Web3 e HexString
setDErc20 x0 r = uncurryFields  r $ setDErc20' x0
   where
    setDErc20' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newErc20") Address -> Web3 e HexString
    setDErc20' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDErc20Fn)

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