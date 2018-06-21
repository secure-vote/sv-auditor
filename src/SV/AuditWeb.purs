module SV.AuditWeb (main, testArgs, version) where

import SV.Prelude

import Control.Monad.Aff (launchAff_, message)
import Control.Monad.Aff.Console as AffC
import Control.Monad.Error.Class (catchError)
import Data.Argonaut.Core as J
import Data.Int (toNumber)
import Data.Map (Map, toUnfoldable)
import Data.StrMap as SMap
import Data.StrMap as StrMap
import Global.Unsafe (unsafeStringify)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Partial.Unsafe (unsafePartial)
import SV.Light.AuditApp (app, AppArgs)
import SV.Types.OutboundLogs (SUAux(..), OutAllDeets)
import SV.Utils.BigNumber (bnToStr)
import Simple.JSON (write, writeJSON)


sToAddrUnsafe = unsafePartial fromJust <<< (mkAddress <=< mkHexString)

testArgs =
    { ethUrls: StrMap.fromFoldable [Tuple "1" "https://mainnet.eth.secure.vote/auditorDevArgs", Tuple "42" "https://kovan.eth.secure.vote/auditorDevArgs"]
    , indexEns: "index-v1.kov.sv"
    , startingNetwork: "42"
    , ensDetails: StrMap.fromFoldable [Tuple "1" (sToAddrUnsafe "0x314159265dD8dbb310642f98f50C066173C1259b"), Tuple "42" (sToAddrUnsafe "0xd6F4f22eeC158c434b17d01f62f5dF33b108Ae93")]
    , ballotId: "50282030322211705750201517969034130535463358470884956019831293948856"
    , dev: true
    }


version = "2.0.0"


main :: forall a e eff. AppArgs -> (J.Json -> Unit) -> Eff _ Unit
main args updateF = launchAff_ $ do
    let updateF_ = updateF2 updateF
    let failErrorCode = 1
    AffC.log $ "AuditWeb starting with args: " <> unsafeStringify args
    _ <- catchError (app args updateF_) \e -> do
                let _ = updateF_ {t: "fail", p: SuStr $ message e}
                pure $ Left $ Tuple failErrorCode $ message e
    pure unit


-- | Transform StatusUpdate values into JSON values with one of the structures: (for inter-compatibility with elm)
-- | Log Message: `{t: "log", p: <msg :: String>}`
-- | Fail Message: `{t: "fail", p: <errMsg :: String>}`
-- | Success Message `{t: "success", p :: {nVotes :: Number, totals :: { <optionName :: String>: <totalAsEncodedDecimal :: String> }}}`
-- | Balances Message `{t: "balances", p :: StrMap <toString Address> <toString Decimal>}`
-- | Delegations Message `{t: "delegations", p :: StrMap <toString Address> <toString Address>}`
updateF2 :: (_ -> Unit) -> {t :: String, p :: SUAux} -> Unit
updateF2 f ({t, p}) = case p of
    SuStr p_ -> f $ toJson J.fromString t p_
    SuRes p_ -> f $ toJson procRes t p_
    SuBal p_ -> f $ toJson balToJson t p_
    SuDlgt p_ -> f $ toJson dlgtToJson t p_
  where
    toJson :: forall b. (b -> J.Json) -> String -> b -> J.Json
    toJson processP t p = J.fromObject $ SMap.insert "p" (processP p) $ SMap.singleton "t" $ J.fromString t

    procRes :: OutAllDeets -> J.Json
    procRes {nVotes, ballotResults} =
            J.fromObject
            $ SMap.insert "nVotes" (J.fromNumber $ toNumber nVotes)
            $ SMap.singleton "totals" $ J.fromObject $ convertCountToStr <$> ballotResults
    convertCountToStr {count, nVotes} = J.fromObject $ SMap.fromFoldable [Tuple "count" $ J.fromString count, Tuple "nVotes" $ J.fromNumber $ toNumber nVotes]

    balToJson :: Map Address BigNumber -> J.Json
    balToJson = (toUnfoldable :: _ -> Array _)
                >>> map (\(Tuple addr bal) -> Tuple (show addr) (J.fromString $ bnToStr bal))
                >>> SMap.fromFoldable >>> J.fromObject

    dlgtToJson :: Map Address Address -> J.Json
    dlgtToJson = (toUnfoldable :: _ -> Array _)
                 >>> map (\(Tuple v d) -> Tuple (show v) (J.fromString $ show d))
                 >>> SMap.fromFoldable >>> J.fromObject
