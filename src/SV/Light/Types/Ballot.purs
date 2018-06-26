module SV.Light.Types.Ballot where

import SV.Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError, runExcept)
import Data.Array (length)
import Data.Foreign (Foreign, F)
import Data.Newtype (class Newtype)
import Data.Record.ShowRecord (class ShowRowList, showRecord)
import Debug.Trace (spy)
import Global.Unsafe (unsafeStringify)
import Network.Ethereum.Web3.Solidity.Sizes (s1)
import Network.Ethereum.Web3.Types (Address, HexString, mkAddress, mkHexString)
import SV.Light.Types.Never (Never)
import SecureVote.Utils.Json (mkFErr)
import Simple.JSON (class ReadForeign, read', readImpl)
import Type.Row (class RowToList)


data BallotSpec
    = BVer01 BSpec01Impl
    | BVer02 BSpec02Impl OptsOuter

derive instance eqBallotSpec :: Eq BallotSpec
instance showBSpec :: Show BallotSpec where
    show bs = case bs of
        BVer01 a -> showB "01" a
        BVer02 b o -> showB "02" {ballot: b, options: o}
      where
        showB :: forall fs rs. RowToList fs rs => ShowRowList rs fs => String -> Record ( | fs) -> String
        showB vStr rec = "( BallotSpec Version " <> vStr <> " => " <> showRecord rec <> " )"


data BallotSpecChoice
    = BChoice01
    | BChoice02


bSpecChoiceToStr :: BallotSpecChoice -> String
bSpecChoiceToStr c =
    case c of
        BChoice01 -> "(Deprecated) Ballot (v01)"
        BChoice02 -> "Standard Ballot (v02)"


getTitle :: BallotSpec -> String
getTitle b =
    case b of
        BVer01 d -> d.ballotTitle
        BVer02 d o -> d.ballotTitle


type BSpec01Impl =
    { ballotTitle :: String
    , shortDesc :: String
    , longDesc :: String
    , startTime :: Int
    , endTime :: Int
    , erc20Addr :: Address
    , discussionLink :: Maybe String
    , binding :: Boolean
    , encryptionPK :: Maybe HexString
    , options :: OptsOuter
    }


type BSpec02Impl =
    { ballotTitle :: String
    , shortDesc :: String
    , longDesc :: String
    , subgroup :: Maybe Never
    , discussionLink :: Maybe String
    , encryptionPK :: Maybe HexString
    }


data OptsOuter
    = OptsSimple SimpleVer (Array SimpleOption)
    | OptsBinary
    | OptsPetition


derive instance eqOptsOuter :: Eq OptsOuter
instance showOptsOuter :: Show OptsOuter where
    show oo = (\s -> "( OptsOuter " <> s <> " )") $ case oo of
        OptsBinary -> "OptsBinary"
        OptsSimple v os -> (\s -> "( OptsSimple " <> s <> " )") $ case v of
            RangeVotingPlusMinus3 -> "RangeVotingPlusMinus3 ( " <> show (showSimpleOpt <$> os) <> " )"
        OptsPetition -> "OptsPetition"


optsNOptions :: OptsOuter -> Int
optsNOptions os =
    case os of
        OptsSimple RangeVotingPlusMinus3 xs -> length xs
        OptsBinary -> 1
        OptsPetition -> 1


data OptsChoice
    = OChSimpleRange
    | OChBinary
    | OChPetition


oChoiceToStr :: OptsChoice -> String
oChoiceToStr o =
    case o of
        OChSimpleRange -> "Range Voting"
        OChBinary -> "Binary Yes/No"
        OChPetition -> "Petition"


newtype SimpleOption = SimpleOption
    { optionTitle :: String, optionDesc :: Maybe String }

derive instance eqSimpleOption :: Eq SimpleOption
derive instance ntSimpleOption :: Newtype SimpleOption _

showSimpleOpt :: SimpleOption -> String
showSimpleOpt (SimpleOption {optionTitle, optionDesc}) = "[ Title: " <> optionTitle <> ", Desc: " <> show optionDesc <> " ]"


data SimpleVer
    = RangeVotingPlusMinus3

derive instance eqSimpleV :: Eq SimpleVer

type ReadBSpecStage1Row = ( ballotVersion :: Int, ballotInner :: Foreign )
type ReadBSpecStage1 = { | ReadBSpecStage1Row }
type ReadBSpecWOptsStage1 = { optionsVersion :: Int, ballotInner :: Foreign }

type ReadOptsOuterBV1 = { optionsVersion :: Int, options :: Maybe Foreign }
type ReadOptsOuterBV2 = { optionsVersion :: Int, optionsInner :: {options :: Maybe Foreign, aux :: Maybe Foreign}}


instance readFBallotSpec :: ReadForeign BallotSpec where
    readImpl a = do
        (s1 :: ReadBSpecStage1) <- read' a
        case s1.ballotVersion of
            1 -> b01Conv =<< read' s1.ballotInner
            2 -> join $ (b02Conv <$> read' a <*> (read' a))
            _ -> mkFErr $ "Invalid BallotSpec: " <> unsafeStringify a
      where
        b01Conv b = do
            let {encryptionPK, erc20Addr} = b
            (erc_ :: Address) <- fromMaybe (mkFErr "Cannot convert erc20Addr to addr") (pure <$> (mkAddress =<< mkHexString =<< erc20Addr :: Maybe String))
            encPk_ <- strToHexIfJust encryptionPK
            pure $ BVer01 $ b {encryptionPK = encPk_, erc20Addr = erc_}
        b02Conv (a' :: ReadBSpecWOptsStage1) (o :: OptsOuter) = do
            b <- read' a'.ballotInner
            encPK <- strToHexIfJust b.encryptionPK
            pure $ BVer02 (b {encryptionPK = encPK}) o
        mkExcept = pure
        -- | this only throws if the _conversion_ to `Maybe HexString` fails; `Nothing`s just pass through
        strToHexIfJust :: Maybe String -> F (Maybe HexString)
        strToHexIfJust = maybe (pure Nothing) (fromMaybe (mkFErr "Cannot convert encPK to HexString") <<< (map (pure <<< Just) <<< mkHexString))


instance readFOptsOuter :: ReadForeign OptsOuter where
    readImpl a = do
        catchError (readOptsBV1 a) (\e -> readOptsBV2 a <|> throwError e)
      where
        readOptsBV1 a = do
            s1 :: ReadOptsOuterBV1 <- read' a
            mkOptsOuter s1.optionsVersion s1.options
        readOptsBV2 a = do
            s1 :: ReadOptsOuterBV2 <- read' a
            reqNothing "aux" s1.optionsInner.aux
            optsOuter <- mkOptsOuter s1.optionsVersion s1.optionsInner.options
            pure optsOuter
        mkOptsOuter ver opts = case ver of
                1 -> opt01Conv =<< read' =<< (fromMaybe (mkFErr "OptionsV01 expected SimpleOptions but got Nothing") (pure <$> opts))
                2 -> reqNothing' opts *> pure OptsBinary
                3 -> reqNothing' opts *> pure OptsPetition
                _ -> mkFErr $ "Invalid OptionsVersion " <> show ver
        opt01Conv options = do
            opts :: Array SimpleOption <- read' options
            pure $ OptsSimple RangeVotingPlusMinus3 opts
        reqNothing' = reqNothing "options"

reqNothing :: forall a. String -> Maybe a -> F Unit
reqNothing name m = if isNothing m then pure unit else mkFErr $ "Expected nothing for `" <> name <> "` but got: " <> unsafeStringify m


instance readSimpleOption :: ReadForeign SimpleOption where
    readImpl a = do
        SimpleOption <$> read' a
