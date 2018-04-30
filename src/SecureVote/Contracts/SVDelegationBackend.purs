--------------------------------------------------------------------------------
-- | SVDelegationBackend
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVDelegationBackend where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4, Tuple6, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | CreateEthTknDelegationFn
--------------------------------------------------------------------------------


type CreateEthTknDelegationFn = Tagged (SProxy "createEthTknDelegation(address,address,address)") (Tuple3 Address Address Address)

createEthTknDelegation :: forall e. TransactionOptions NoPay -> { voter :: Address, delegate :: Address, tokenContract :: Address } -> Web3 e HexString
createEthTknDelegation x0 r = uncurryFields  r $ createEthTknDelegation' x0
   where
    createEthTknDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "voter") Address -> Tagged (SProxy "delegate") Address -> Tagged (SProxy "tokenContract") Address -> Web3 e HexString
    createEthTknDelegation' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: CreateEthTknDelegationFn)

--------------------------------------------------------------------------------
-- | FnT_knownNamespacesFn
--------------------------------------------------------------------------------


type FnT_knownNamespacesFn = Tagged (SProxy "_knownNamespaces(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

_knownNamespaces :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError Boolean)
_knownNamespaces x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: FnT_knownNamespacesFn)

--------------------------------------------------------------------------------
-- | TotalDelegationsFn
--------------------------------------------------------------------------------


type TotalDelegationsFn = Tagged (SProxy "totalDelegations()") (Tuple0 )

totalDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
totalDelegations x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalDelegationsFn)

--------------------------------------------------------------------------------
-- | V1DlgtSCFn
--------------------------------------------------------------------------------


type V1DlgtSCFn = Tagged (SProxy "v1DlgtSC()") (Tuple0 )

v1DlgtSC :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
v1DlgtSC x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: V1DlgtSCFn)

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
-- | FnT_logNamespacesFn
--------------------------------------------------------------------------------


type FnT_logNamespacesFn = Tagged (SProxy "_logNamespaces(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

_logNamespaces :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
_logNamespaces x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: FnT_logNamespacesFn)

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
-- | CurrAdminEpochFn
--------------------------------------------------------------------------------


type CurrAdminEpochFn = Tagged (SProxy "currAdminEpoch()") (Tuple0 )

currAdminEpoch :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
currAdminEpoch x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CurrAdminEpochFn)

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
-- | FnT_forgetDelegationsBeforeFn
--------------------------------------------------------------------------------


type FnT_forgetDelegationsBeforeFn = Tagged (SProxy "_forgetDelegationsBefore(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

_forgetDelegationsBefore :: forall e. TransactionOptions NoPay -> ChainCursor -> (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
_forgetDelegationsBefore x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: FnT_forgetDelegationsBeforeFn)

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
-- | FnT_allDelegationsFn
--------------------------------------------------------------------------------


type FnT_allDelegationsFn = Tagged (SProxy "_allDelegations(uint64)") (Tuple1 (UIntN (D6 :& DOne D4)))

_allDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D6 :& DOne D4)) -> Web3 e (Either CallError (Tuple6 (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
_allDelegations x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: FnT_allDelegationsFn)

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
-- | FindPossibleDelegatorsOfRawFn
--------------------------------------------------------------------------------


type FindPossibleDelegatorsOfRawFn = Tagged (SProxy "findPossibleDelegatorsOfRaw(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

findPossibleDelegatorsOfRaw :: forall e. TransactionOptions NoPay -> ChainCursor -> { delegate :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple2 (Array (BytesN (D3 :& DOne D2))) (Array (UIntN (D2 :& D5 :& DOne D6)))))
findPossibleDelegatorsOfRaw x0 cm r = uncurryFields  r $ findPossibleDelegatorsOfRaw' x0 cm
   where
    findPossibleDelegatorsOfRaw' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "delegate") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple2 (Array (BytesN (D3 :& DOne D2))) (Array (UIntN (D2 :& D5 :& DOne D6)))))
    findPossibleDelegatorsOfRaw' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: FindPossibleDelegatorsOfRawFn)

--------------------------------------------------------------------------------
-- | AdminLockdownFn
--------------------------------------------------------------------------------


type AdminLockdownFn = Tagged (SProxy "adminLockdown()") (Tuple0 )

adminLockdown :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
adminLockdown x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AdminLockdownFn)

--------------------------------------------------------------------------------
-- | FnT_getIdIfValidFn
--------------------------------------------------------------------------------


type FnT_getIdIfValidFn = Tagged (SProxy "_getIdIfValid(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

_getIdIfValid :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: (BytesN (D3 :& DOne D2)), namespace :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
_getIdIfValid x0 cm r = uncurryFields  r $ _getIdIfValid' x0 cm
   where
    _getIdIfValid' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "namespace") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
    _getIdIfValid' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: FnT_getIdIfValidFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RawDelegationsFn
--------------------------------------------------------------------------------


type RawDelegationsFn = Tagged (SProxy "rawDelegations(uint256,bytes32)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (BytesN (D3 :& DOne D2)))

rawDelegations :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
rawDelegations x0 cm x2 x3 = map unTuple1 <$> call x0 cm ((tagged $ Tuple2 x2 x3) :: RawDelegationsFn)

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
-- | ResetAllDelegationsFn
--------------------------------------------------------------------------------


type ResetAllDelegationsFn = Tagged (SProxy "resetAllDelegations(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

resetAllDelegations :: forall e. TransactionOptions NoPay -> { voter :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
resetAllDelegations x0 r = uncurryFields  r $ resetAllDelegations' x0
   where
    resetAllDelegations' :: TransactionOptions NoPay -> Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    resetAllDelegations' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: ResetAllDelegationsFn)

--------------------------------------------------------------------------------
-- | CreateEthGlobalDelegationFn
--------------------------------------------------------------------------------


type CreateEthGlobalDelegationFn = Tagged (SProxy "createEthGlobalDelegation(address,address)") (Tuple2 Address Address)

createEthGlobalDelegation :: forall e. TransactionOptions NoPay -> { voter :: Address, delegate :: Address } -> Web3 e HexString
createEthGlobalDelegation x0 r = uncurryFields  r $ createEthGlobalDelegation' x0
   where
    createEthGlobalDelegation' :: TransactionOptions NoPay -> Tagged (SProxy "voter") Address -> Tagged (SProxy "delegate") Address -> Web3 e HexString
    createEthGlobalDelegation' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: CreateEthGlobalDelegationFn)

--------------------------------------------------------------------------------
-- | FnT_delegationSigsFn
--------------------------------------------------------------------------------


type FnT_delegationSigsFn = Tagged (SProxy "_delegationSigs(uint64)") (Tuple1 (UIntN (D6 :& DOne D4)))

_delegationSigs :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D6 :& DOne D4)) -> Web3 e (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2))))
_delegationSigs x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: FnT_delegationSigsFn)

--------------------------------------------------------------------------------
-- | FindPossibleDelegatorsOfFn
--------------------------------------------------------------------------------


type FindPossibleDelegatorsOfFn = Tagged (SProxy "findPossibleDelegatorsOf(address)") (Tuple1 Address)

findPossibleDelegatorsOf :: forall e. TransactionOptions NoPay -> ChainCursor -> { delegate :: Address } -> Web3 e (Either CallError (Tuple2 (Array Address) (Array Address)))
findPossibleDelegatorsOf x0 cm r = uncurryFields  r $ findPossibleDelegatorsOf' x0 cm
   where
    findPossibleDelegatorsOf' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "delegate") Address -> Web3 e (Either CallError (Tuple2 (Array Address) (Array Address)))
    findPossibleDelegatorsOf' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: FindPossibleDelegatorsOfFn)

--------------------------------------------------------------------------------
-- | ResolveRawDelegationFn
--------------------------------------------------------------------------------


type ResolveRawDelegationFn = Tagged (SProxy "resolveRawDelegation(bytes32,uint256)") (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)))

resolveRawDelegation :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: (BytesN (D3 :& DOne D2)), namespace :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple4 (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
resolveRawDelegation x0 cm r = uncurryFields  r $ resolveRawDelegation' x0 cm
   where
    resolveRawDelegation' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "namespace") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple4 (UIntN (D6 :& DOne D4)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
    resolveRawDelegation' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 (untagged y2 ) (untagged y3 )) :: ResolveRawDelegationFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _prevSC :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_prevSC") Address -> Web3 e HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 (untagged y2 )) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | NewDelegation
--------------------------------------------------------------------------------


newtype NewDelegation = NewDelegation {voter :: (BytesN (D3 :& DOne D2)),id :: (UIntN (D6 :& DOne D4))}

derive instance newtypeNewDelegation :: Newtype NewDelegation _

instance eventFilterNewDelegation :: EventFilter NewDelegation where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "905cab43c8084f52609d05a8421d7e66ddfc6c5b41ec52461b431f59c8515e14"),Nothing]

instance indexedEventNewDelegation :: IndexedEvent (Tuple1 (Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "id") (UIntN (D6 :& DOne D4)))) NewDelegation where
  isAnonymous _ = false

derive instance genericNewDelegation :: Generic NewDelegation _

instance eventGenericNewDelegationShow :: Show NewDelegation where
	show = genericShow

instance eventGenericNewDelegationeq :: Eq NewDelegation where
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