module SV.Types.Lenses where

import SV.Prelude

import Data.Lens (Lens', lens)
import Network.Ethereum.Web3 (Address, HexString)
import SV.Light.Types.Ballot (BSpec01Impl, BallotSpec(..), OptsOuter, BSpec02Impl)
import SecureVote.Web3.Web3 (zeroAddr)


type BSpecGetters a =
    { gV01 :: BSpec01Impl -> a
    , gV02 :: BSpec02Impl -> OptsOuter -> a
    }


type BSpecSetters a =
    { sV01 :: BSpec01Impl -> a -> BSpec01Impl
    , sV02 :: BSpec02Impl -> OptsOuter -> a -> Tuple BSpec02Impl OptsOuter
    }


getFromBSpec :: forall a. BSpecGetters a -> BallotSpec -> a
getFromBSpec {gV01, gV02} b = case b of
    BVer01 bs -> gV01 bs
    BVer02 b o -> gV02 b o


getBSpecVer :: BallotSpec -> Int
getBSpecVer b = case b of
    BVer01 _ -> 1
    BVer02 _ _ -> 2


setFromBSpec :: forall a. BSpecSetters a -> BallotSpec -> a -> BallotSpec
setFromBSpec {sV01, sV02} b thing = case b of
    BVer01 bs -> BVer01 $ sV01 bs thing
    BVer02 b o -> (sV02 b o thing) # \(Tuple b' o') -> BVer02 b' o'


_erc20Addr :: Lens' BallotSpec Address
_erc20Addr = lens
    (getFromBSpec {gV01: \r -> r.erc20Addr, gV02: \b o -> zeroAddr})
    (setFromBSpec {sV01: (\bs addr -> bs {erc20Addr = addr}), sV02: \b o addr -> Tuple b o})


_endTime :: Lens' BallotSpec Int
_endTime = lens
    (getFromBSpec {gV01: (\r -> r.endTime), gV02: \b o -> 0})
    (setFromBSpec {sV01: (\bs et -> bs {endTime = et}), sV02: \b o e -> Tuple b o})

_encryptionPK :: Lens' BallotSpec (Maybe HexString)
_encryptionPK = lens
    (getFromBSpec {gV01: g, gV02: \b o -> g b})
    (setFromBSpec
        { sV01: (\r encpk -> r {encryptionPK = encpk})
        , sV02: \b o epk -> Tuple (b {encryptionPK = epk}) o
        })
  where
    g :: forall rs. {encryptionPK :: Maybe HexString | rs} -> Maybe HexString
    g {encryptionPK} = encryptionPK

_options :: Lens' BallotSpec OptsOuter
_options = lens
    (getFromBSpec {gV01: \r -> r.options, gV02: \b o -> o})
    (setFromBSpec {sV01: \r opts -> r {options = opts}, sV02: \b o o2 -> Tuple b o2})
