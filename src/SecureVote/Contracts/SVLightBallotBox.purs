--------------------------------------------------------------------------------
-- | SVLightBallotBox
--------------------------------------------------------------------------------

module SecureVote.Contracts.SVLightBallotBox where

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
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple6, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | NVotesCastFn
--------------------------------------------------------------------------------


type NVotesCastFn = Tagged (SProxy "nVotesCast()") (Tuple0 )

nVotesCast :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nVotesCast x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NVotesCastFn)

--------------------------------------------------------------------------------
-- | GetSignatureFn
--------------------------------------------------------------------------------


type GetSignatureFn = Tagged (SProxy "getSignature(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getSignature :: forall e. TransactionOptions NoPay -> ChainCursor -> { id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Vector (DOne D2) (BytesN (D3 :& DOne D2))))
getSignature x0 cm r = uncurryFields  r $ getSignature' x0 cm
   where
    getSignature' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Vector (DOne D2) (BytesN (D3 :& DOne D2))))
    getSignature' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetSignatureFn)

--------------------------------------------------------------------------------
-- | GetVersionFn
--------------------------------------------------------------------------------


type GetVersionFn = Tagged (SProxy "getVersion()") (Tuple0 )

getVersion :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getVersion x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetVersionFn)

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
-- | SubmitBallotSignedWithEncFn
--------------------------------------------------------------------------------


type SubmitBallotSignedWithEncFn = Tagged (SProxy "submitBallotSignedWithEnc(bytes32,bytes32,bytes32,bytes32[2])") (Tuple4 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (Vector (DOne D2) (BytesN (D3 :& DOne D2))))

submitBallotSignedWithEnc :: forall e. TransactionOptions NoPay -> { ballot :: (BytesN (D3 :& DOne D2)), curve25519PK :: (BytesN (D3 :& DOne D2)), ed25519PK :: (BytesN (D3 :& DOne D2)), signature :: (Vector (DOne D2) (BytesN (D3 :& DOne D2))) } -> Web3 e HexString
submitBallotSignedWithEnc x0 r = uncurryFields  r $ submitBallotSignedWithEnc' x0
   where
    submitBallotSignedWithEnc' :: TransactionOptions NoPay -> Tagged (SProxy "ballot") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "curve25519PK") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "ed25519PK") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "signature") (Vector (DOne D2) (BytesN (D3 :& DOne D2))) -> Web3 e HexString
    submitBallotSignedWithEnc' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 (untagged y1 ) (untagged y2 ) (untagged y3 ) (untagged y4 )) :: SubmitBallotSignedWithEncFn)

--------------------------------------------------------------------------------
-- | GetBallotsSignedFromFn
--------------------------------------------------------------------------------


type GetBallotsSignedFromFn = Tagged (SProxy "getBallotsSignedFrom(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

getBallotsSignedFrom :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: (BytesN (D3 :& DOne D2)) } -> Web3 e (Either CallError (Tuple6 (Array (UIntN (D2 :& D5 :& DOne D6))) (Array (BytesN (D3 :& DOne D2))) (Array (UIntN (D3 :& DOne D2))) (Array (BytesN (D3 :& DOne D2))) (Array (Vector (DOne D2) (BytesN (D3 :& DOne D2)))) Boolean))
getBallotsSignedFrom x0 cm r = uncurryFields  r $ getBallotsSignedFrom' x0 cm
   where
    getBallotsSignedFrom' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)) -> Web3 e (Either CallError (Tuple6 (Array (UIntN (D2 :& D5 :& DOne D6))) (Array (BytesN (D3 :& DOne D2))) (Array (UIntN (D3 :& DOne D2))) (Array (BytesN (D3 :& DOne D2))) (Array (Vector (DOne D2) (BytesN (D3 :& DOne D2)))) Boolean))
    getBallotsSignedFrom' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetBallotsSignedFromFn)

--------------------------------------------------------------------------------
-- | GetCreationBlockFn
--------------------------------------------------------------------------------


type GetCreationBlockFn = Tagged (SProxy "getCreationBlock()") (Tuple0 )

getCreationBlock :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
getCreationBlock x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetCreationBlockFn)

--------------------------------------------------------------------------------
-- | BallotEncryptionSeckeyFn
--------------------------------------------------------------------------------


type BallotEncryptionSeckeyFn = Tagged (SProxy "ballotEncryptionSeckey()") (Tuple0 )

ballotEncryptionSeckey :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
ballotEncryptionSeckey x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: BallotEncryptionSeckeyFn)

--------------------------------------------------------------------------------
-- | GetEncSeckeyFn
--------------------------------------------------------------------------------


type GetEncSeckeyFn = Tagged (SProxy "getEncSeckey()") (Tuple0 )

getEncSeckey :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getEncSeckey x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetEncSeckeyFn)

--------------------------------------------------------------------------------
-- | GetEndTimeFn
--------------------------------------------------------------------------------


type GetEndTimeFn = Tagged (SProxy "getEndTime()") (Tuple0 )

getEndTime :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
getEndTime x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetEndTimeFn)

--------------------------------------------------------------------------------
-- | SubmitBallotSignedNoEncFn
--------------------------------------------------------------------------------


type SubmitBallotSignedNoEncFn = Tagged (SProxy "submitBallotSignedNoEnc(bytes32,bytes32,bytes32[2])") (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (Vector (DOne D2) (BytesN (D3 :& DOne D2))))

submitBallotSignedNoEnc :: forall e. TransactionOptions NoPay -> { ballot :: (BytesN (D3 :& DOne D2)), ed25519PK :: (BytesN (D3 :& DOne D2)), signature :: (Vector (DOne D2) (BytesN (D3 :& DOne D2))) } -> Web3 e HexString
submitBallotSignedNoEnc x0 r = uncurryFields  r $ submitBallotSignedNoEnc' x0
   where
    submitBallotSignedNoEnc' :: TransactionOptions NoPay -> Tagged (SProxy "ballot") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "ed25519PK") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "signature") (Vector (DOne D2) (BytesN (D3 :& DOne D2))) -> Web3 e HexString
    submitBallotSignedNoEnc' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 (untagged y1 ) (untagged y2 ) (untagged y3 )) :: SubmitBallotSignedNoEncFn)

--------------------------------------------------------------------------------
-- | HasVotedEthFn
--------------------------------------------------------------------------------


type HasVotedEthFn = Tagged (SProxy "hasVotedEth(address)") (Tuple1 Address)

hasVotedEth :: forall e. TransactionOptions NoPay -> ChainCursor -> { v :: Address } -> Web3 e (Either CallError Boolean)
hasVotedEth x0 cm r = uncurryFields  r $ hasVotedEth' x0 cm
   where
    hasVotedEth' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "v") Address -> Web3 e (Either CallError Boolean)
    hasVotedEth' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: HasVotedEthFn)

--------------------------------------------------------------------------------
-- | GetSubmissionBitsFn
--------------------------------------------------------------------------------


type GetSubmissionBitsFn = Tagged (SProxy "getSubmissionBits()") (Tuple0 )

getSubmissionBits :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D1 :& DOne D6)))
getSubmissionBits x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetSubmissionBitsFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | IsOfficialFn
--------------------------------------------------------------------------------


type IsOfficialFn = Tagged (SProxy "isOfficial()") (Tuple0 )

isOfficial :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
isOfficial x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsOfficialFn)

--------------------------------------------------------------------------------
-- | SetDeprecatedFn
--------------------------------------------------------------------------------


type SetDeprecatedFn = Tagged (SProxy "setDeprecated()") (Tuple0 )

setDeprecated :: forall e. TransactionOptions NoPay -> Web3 e HexString
setDeprecated x0 = sendTx x0 ((tagged $ Tuple0 ) :: SetDeprecatedFn)

--------------------------------------------------------------------------------
-- | IsBindingFn
--------------------------------------------------------------------------------


type IsBindingFn = Tagged (SProxy "isBinding()") (Tuple0 )

isBinding :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
isBinding x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsBindingFn)

--------------------------------------------------------------------------------
-- | GetBallotsEthFromFn
--------------------------------------------------------------------------------


type GetBallotsEthFromFn = Tagged (SProxy "getBallotsEthFrom(address)") (Tuple1 Address)

getBallotsEthFrom :: forall e. TransactionOptions NoPay -> ChainCursor -> { voter :: Address } -> Web3 e (Either CallError (Tuple6 (Array (UIntN (D2 :& D5 :& DOne D6))) (Array (BytesN (D3 :& DOne D2))) (Array (UIntN (D3 :& DOne D2))) (Array (BytesN (D3 :& DOne D2))) (Array (Vector (DOne D2) (BytesN (D3 :& DOne D2)))) Boolean))
getBallotsEthFrom x0 cm r = uncurryFields  r $ getBallotsEthFrom' x0 cm
   where
    getBallotsEthFrom' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "voter") Address -> Web3 e (Either CallError (Tuple6 (Array (UIntN (D2 :& D5 :& DOne D6))) (Array (BytesN (D3 :& DOne D2))) (Array (UIntN (D3 :& DOne D2))) (Array (BytesN (D3 :& DOne D2))) (Array (Vector (DOne D2) (BytesN (D3 :& DOne D2)))) Boolean))
    getBallotsEthFrom' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetBallotsEthFromFn)

--------------------------------------------------------------------------------
-- | GetBallotSignedFn
--------------------------------------------------------------------------------


type GetBallotSignedFn = Tagged (SProxy "getBallotSigned(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getBallotSigned :: forall e. TransactionOptions NoPay -> ChainCursor -> { id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D3 :& DOne D2))))
getBallotSigned x0 cm r = uncurryFields  r $ getBallotSigned' x0 cm
   where
    getBallotSigned' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D3 :& DOne D2))))
    getBallotSigned' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetBallotSignedFn)

--------------------------------------------------------------------------------
-- | Ed25519SignaturesFn
--------------------------------------------------------------------------------


type Ed25519SignaturesFn = Tagged (SProxy "ed25519Signatures(uint256,uint256)") (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)))

ed25519Signatures :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
ed25519Signatures x0 cm x2 x3 = map unTuple1 <$> call x0 cm ((tagged $ Tuple2 x2 x3) :: Ed25519SignaturesFn)

--------------------------------------------------------------------------------
-- | BallotsSignedFn
--------------------------------------------------------------------------------


type BallotsSignedFn = Tagged (SProxy "ballotsSigned(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

ballotsSigned :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D3 :& DOne D2))))
ballotsSigned x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: BallotsSignedFn)

--------------------------------------------------------------------------------
-- | GetBallotEthFn
--------------------------------------------------------------------------------


type GetBallotEthFn = Tagged (SProxy "getBallotEth(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getBallotEth :: forall e. TransactionOptions NoPay -> ChainCursor -> { id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) Address (UIntN (D3 :& DOne D2))))
getBallotEth x0 cm r = uncurryFields  r $ getBallotEth' x0 cm
   where
    getBallotEth' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) Address (UIntN (D3 :& DOne D2))))
    getBallotEth' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetBallotEthFn)

--------------------------------------------------------------------------------
-- | BallotsEthFn
--------------------------------------------------------------------------------


type BallotsEthFn = Tagged (SProxy "ballotsEth(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

ballotsEth :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) Address (UIntN (D3 :& DOne D2))))
ballotsEth x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: BallotsEthFn)

--------------------------------------------------------------------------------
-- | SubmitBallotWithPkFn
--------------------------------------------------------------------------------


type SubmitBallotWithPkFn = Tagged (SProxy "submitBallotWithPk(bytes32,bytes32)") (Tuple2 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)))

submitBallotWithPk :: forall e. TransactionOptions NoPay -> { ballot :: (BytesN (D3 :& DOne D2)), encPK :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
submitBallotWithPk x0 r = uncurryFields  r $ submitBallotWithPk' x0
   where
    submitBallotWithPk' :: TransactionOptions NoPay -> Tagged (SProxy "ballot") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "encPK") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    submitBallotWithPk' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 (untagged y1 ) (untagged y2 )) :: SubmitBallotWithPkFn)

--------------------------------------------------------------------------------
-- | IsDeprecatedFn
--------------------------------------------------------------------------------


type IsDeprecatedFn = Tagged (SProxy "isDeprecated()") (Tuple0 )

isDeprecated :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
isDeprecated x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsDeprecatedFn)

--------------------------------------------------------------------------------
-- | GetStartTimeFn
--------------------------------------------------------------------------------


type GetStartTimeFn = Tagged (SProxy "getStartTime()") (Tuple0 )

getStartTime :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D6 :& DOne D4)))
getStartTime x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetStartTimeFn)

--------------------------------------------------------------------------------
-- | RevealSeckeyFn
--------------------------------------------------------------------------------


type RevealSeckeyFn = Tagged (SProxy "revealSeckey(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

revealSeckey :: forall e. TransactionOptions NoPay -> { _secKey :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
revealSeckey x0 r = uncurryFields  r $ revealSeckey' x0
   where
    revealSeckey' :: TransactionOptions NoPay -> Tagged (SProxy "_secKey") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    revealSeckey' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: RevealSeckeyFn)

--------------------------------------------------------------------------------
-- | IsTestingFn
--------------------------------------------------------------------------------


type IsTestingFn = Tagged (SProxy "isTesting()") (Tuple0 )

isTesting :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
isTesting x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsTestingFn)

--------------------------------------------------------------------------------
-- | SubmitBallotNoPkFn
--------------------------------------------------------------------------------


type SubmitBallotNoPkFn = Tagged (SProxy "submitBallotNoPk(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

submitBallotNoPk :: forall e. TransactionOptions NoPay -> { ballot :: (BytesN (D3 :& DOne D2)) } -> Web3 e HexString
submitBallotNoPk x0 r = uncurryFields  r $ submitBallotNoPk' x0
   where
    submitBallotNoPk' :: TransactionOptions NoPay -> Tagged (SProxy "ballot") (BytesN (D3 :& DOne D2)) -> Web3 e HexString
    submitBallotNoPk' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SubmitBallotNoPkFn)

--------------------------------------------------------------------------------
-- | GetSpecHashFn
--------------------------------------------------------------------------------


type GetSpecHashFn = Tagged (SProxy "getSpecHash()") (Tuple0 )

getSpecHash :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getSpecHash x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetSpecHashFn)

--------------------------------------------------------------------------------
-- | Curve25519PubkeysFn
--------------------------------------------------------------------------------


type Curve25519PubkeysFn = Tagged (SProxy "curve25519Pubkeys(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

curve25519Pubkeys :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
curve25519Pubkeys x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: Curve25519PubkeysFn)

--------------------------------------------------------------------------------
-- | SetEndTimeFn
--------------------------------------------------------------------------------


type SetEndTimeFn = Tagged (SProxy "setEndTime(uint64)") (Tuple1 (UIntN (D6 :& DOne D4)))

setEndTime :: forall e. TransactionOptions NoPay -> { newEndTime :: (UIntN (D6 :& DOne D4)) } -> Web3 e HexString
setEndTime x0 r = uncurryFields  r $ setEndTime' x0
   where
    setEndTime' :: TransactionOptions NoPay -> Tagged (SProxy "newEndTime") (UIntN (D6 :& DOne D4)) -> Web3 e HexString
    setEndTime' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: SetEndTimeFn)

--------------------------------------------------------------------------------
-- | GetPubkeyFn
--------------------------------------------------------------------------------


type GetPubkeyFn = Tagged (SProxy "getPubkey(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getPubkey :: forall e. TransactionOptions NoPay -> ChainCursor -> { id :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getPubkey x0 cm r = uncurryFields  r $ getPubkey' x0 cm
   where
    getPubkey' :: TransactionOptions NoPay -> ChainCursor -> Tagged (SProxy "id") (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
    getPubkey' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 (untagged y2 )) :: GetPubkeyFn)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(bytes32,uint256,address)") (Tuple3 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) Address)

constructor :: forall e. TransactionOptions NoPay -> HexString -> { _specHash :: (BytesN (D3 :& DOne D2)), packed :: (UIntN (D2 :& D5 :& DOne D6)), ix :: Address } -> Web3 e HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> Tagged (SProxy "_specHash") (BytesN (D3 :& DOne D2)) -> Tagged (SProxy "packed") (UIntN (D2 :& D5 :& DOne D6)) -> Tagged (SProxy "ix") Address -> Web3 e HexString
    constructor' y0 bc' y2 y3 y4 = deployContract y0 bc' ((tagged $ Tuple3 (untagged y2 ) (untagged y3 ) (untagged y4 )) :: ConstructorFn)



--------------------------------------------------------------------------------
-- | CreatedBallot
--------------------------------------------------------------------------------


newtype CreatedBallot = CreatedBallot {_specHash :: (BytesN (D3 :& DOne D2)),startTs :: (UIntN (D6 :& DOne D4)),endTs :: (UIntN (D6 :& DOne D4)),submissionBits :: (UIntN (D1 :& DOne D6))}

derive instance newtypeCreatedBallot :: Newtype CreatedBallot _

instance eventFilterCreatedBallot :: EventFilter CreatedBallot where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "40465227f0da5abf3027c0e7bd71878ca5e6eff5a697a11f204b7a9ca11c0bb8")]

instance indexedEventCreatedBallot :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "_specHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "startTs") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "endTs") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "submissionBits") (UIntN (D1 :& DOne D6)))) CreatedBallot where
  isAnonymous _ = false

derive instance genericCreatedBallot :: Generic CreatedBallot _

instance eventGenericCreatedBallotShow :: Show CreatedBallot where
	show = genericShow

instance eventGenericCreatedBalloteq :: Eq CreatedBallot where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SuccessfulVote
--------------------------------------------------------------------------------


newtype SuccessfulVote = SuccessfulVote {voter :: (BytesN (D3 :& DOne D2)),ballotId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSuccessfulVote :: Newtype SuccessfulVote _

instance eventFilterSuccessfulVote :: EventFilter SuccessfulVote where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1cddca38ee8a9081693bd76716de64804f418830edd5f221cead058b1218049b"),Nothing]

instance indexedEventSuccessfulVote :: IndexedEvent (Tuple1 (Tagged (SProxy "voter") (BytesN (D3 :& DOne D2)))) (Tuple1 (Tagged (SProxy "ballotId") (UIntN (D2 :& D5 :& DOne D6)))) SuccessfulVote where
  isAnonymous _ = false

derive instance genericSuccessfulVote :: Generic SuccessfulVote _

instance eventGenericSuccessfulVoteShow :: Show SuccessfulVote where
	show = genericShow

instance eventGenericSuccessfulVoteeq :: Eq SuccessfulVote where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SeckeyRevealed
--------------------------------------------------------------------------------


newtype SeckeyRevealed = SeckeyRevealed {secretKey :: (BytesN (D3 :& DOne D2))}

derive instance newtypeSeckeyRevealed :: Newtype SeckeyRevealed _

instance eventFilterSeckeyRevealed :: EventFilter SeckeyRevealed where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "a69839328d982396193483f2260936b1d1f2109fdde204b27c7ac3c1cfd18db0")]

instance indexedEventSeckeyRevealed :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "secretKey") (BytesN (D3 :& DOne D2)))) SeckeyRevealed where
  isAnonymous _ = false

derive instance genericSeckeyRevealed :: Generic SeckeyRevealed _

instance eventGenericSeckeyRevealedShow :: Show SeckeyRevealed where
	show = genericShow

instance eventGenericSeckeyRevealedeq :: Eq SeckeyRevealed where
	eq = genericEq

--------------------------------------------------------------------------------
-- | TestingEnabled
--------------------------------------------------------------------------------


newtype TestingEnabled = TestingEnabled {}

derive instance newtypeTestingEnabled :: Newtype TestingEnabled _

instance eventFilterTestingEnabled :: EventFilter TestingEnabled where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "641e6b9d2f3c463bec5b5cffe3f5017d9a49ad5543d2962eb746c6a7afa223c5")]

instance indexedEventTestingEnabled :: IndexedEvent (Tuple0 ) (Tuple0 ) TestingEnabled where
  isAnonymous _ = false

derive instance genericTestingEnabled :: Generic TestingEnabled _

instance eventGenericTestingEnabledShow :: Show TestingEnabled where
	show = genericShow

instance eventGenericTestingEnabledeq :: Eq TestingEnabled where
	eq = genericEq

--------------------------------------------------------------------------------
-- | DeprecatedContract
--------------------------------------------------------------------------------


newtype DeprecatedContract = DeprecatedContract {}

derive instance newtypeDeprecatedContract :: Newtype DeprecatedContract _

instance eventFilterDeprecatedContract :: EventFilter DeprecatedContract where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "77563e26f751f6c469d11286ef3f15cb0d2033a8b182387a2a44782019961787")]

instance indexedEventDeprecatedContract :: IndexedEvent (Tuple0 ) (Tuple0 ) DeprecatedContract where
  isAnonymous _ = false

derive instance genericDeprecatedContract :: Generic DeprecatedContract _

instance eventGenericDeprecatedContractShow :: Show DeprecatedContract where
	show = genericShow

instance eventGenericDeprecatedContracteq :: Eq DeprecatedContract where
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