--------------------------------------------------------------------------------
-- | BallotBoxIface
--------------------------------------------------------------------------------

module SecureVote.Contracts.BallotBoxIface where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (BytesN, D1, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple6, UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | GetSignatureFn
--------------------------------------------------------------------------------


type GetSignatureFn = Tagged (SProxy "getSignature(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getSignature :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Vector (DOne D2) (BytesN (D3 :& DOne D2))))
getSignature x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: GetSignatureFn)

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

setOwner :: forall e. TransactionOptions NoPay -> Address -> Web3 e HexString
setOwner x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: SetOwnerFn)

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

hasVotedEth :: forall e. TransactionOptions NoPay -> ChainCursor -> Address -> Web3 e (Either CallError Boolean)
hasVotedEth x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: HasVotedEthFn)

--------------------------------------------------------------------------------
-- | GetSubmissionBitsFn
--------------------------------------------------------------------------------


type GetSubmissionBitsFn = Tagged (SProxy "getSubmissionBits()") (Tuple0 )

getSubmissionBits :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (UIntN (D1 :& DOne D6)))
getSubmissionBits x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetSubmissionBitsFn)

--------------------------------------------------------------------------------
-- | IsOfficialFn
--------------------------------------------------------------------------------


type IsOfficialFn = Tagged (SProxy "isOfficial()") (Tuple0 )

isOfficial :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError Boolean)
isOfficial x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsOfficialFn)

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

getBallotSigned :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) (BytesN (D3 :& DOne D2)) (UIntN (D3 :& DOne D2))))
getBallotSigned x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: GetBallotSignedFn)

--------------------------------------------------------------------------------
-- | GetBallotEthFn
--------------------------------------------------------------------------------


type GetBallotEthFn = Tagged (SProxy "getBallotEth(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getBallotEth :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (Tuple3 (BytesN (D3 :& DOne D2)) Address (UIntN (D3 :& DOne D2))))
getBallotEth x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: GetBallotEthFn)

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
-- | GetPubkeyFn
--------------------------------------------------------------------------------


type GetPubkeyFn = Tagged (SProxy "getPubkey(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

getPubkey :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 e (Either CallError (BytesN (D3 :& DOne D2)))
getPubkey x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: GetPubkeyFn)