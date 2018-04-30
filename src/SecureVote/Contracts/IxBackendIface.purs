--------------------------------------------------------------------------------
-- | IxBackendIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.IxBackendIface where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5, UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
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
-- | GetDemocHashFn
--------------------------------------------------------------------------------


type GetDemocHashFn = Tagged (SProxy "getDemocHash(bytes13)") (Tuple1 (BytesN (D1 :& DOne D3)))

getDemocHash :: forall e. TransactionOptions NoPay -> ChainCursor -> { prefix :: (BytesN (D1 :& DOne D3)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getDemocHash x0 cm r = uncurryFields  r $ getDemocHash' x0 cm
   where
    getDemocHash' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "prefix") (BytesN (D1 :& DOne D3)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getDemocHash' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetDemocHashFn)

--------------------------------------------------------------------------------
-- | UpgradeMeFn
--------------------------------------------------------------------------------


type UpgradeMeFn = Tagged (SProxy "upgradeMe(address)") (Tuple1 Address)

upgradeMe :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
upgradeMe x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: UpgradeMeFn)

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
-- | NBallotsGlobalFn
--------------------------------------------------------------------------------


type NBallotsGlobalFn = Tagged (SProxy "nBallotsGlobal()") (Tuple0 )

nBallotsGlobal :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nBallotsGlobal x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NBallotsGlobalFn)

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
-- | AddBallotFn
--------------------------------------------------------------------------------


type AddBallotFn = Tagged (SProxy "addBallot(bytes32,bytes32,address)") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) Address)

addBallot :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), extraData :: (BytesN (D3 :& DOne D2)), bb :: Address } -> Web3 e HexString
addBallot x0 r = uncurryFields  r $ addBallot' x0
   where
    addBallot' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "extraData") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "bb") Address -> Web3 e HexString
    addBallot' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: AddBallotFn)

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
-- | SetDAdminFn
--------------------------------------------------------------------------------


type SetDAdminFn = Tagged (SProxy "setDAdmin(bytes32,address)") (Tuple2 (BytesN (D3 :& DOne D2)) Address)

setDAdmin :: forall e. TransactionOptions NoPay -> { democHash :: (BytesN (D3 :& DOne D2)), newAdmin :: Address } -> Web3 e HexString
setDAdmin x0 r = uncurryFields  r $ setDAdmin' x0
   where
    setDAdmin' :: TransactionOptions NoPay -> Tagged (SProxy "democHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "newAdmin") Address -> Web3 e HexString
    setDAdmin' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SetDAdminFn)

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

initDemoc :: forall e. TransactionOptions NoPay -> String -> Web3 e HexString
initDemoc x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: InitDemocFn)