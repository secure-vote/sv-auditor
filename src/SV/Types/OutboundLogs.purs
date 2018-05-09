module SV.Types.OutboundLogs where

import SV.Prelude

import Data.Map (Map)
import Data.StrMap (StrMap)
import Network.Ethereum.Web3 (Address, BigNumber)


type OutAllDeets = {nVotes :: Int, ballotResults :: StrMap ({count :: String, nVotes :: Int})}

data SUAux = SuStr String | SuRes OutAllDeets | SuBal (Map Address BigNumber) | SuDlgt (Map Address Address)

type StatusUpdate = {t :: String, p :: SUAux}


mkSUFail :: String -> StatusUpdate
mkSUFail e = {t: "fail", p: SuStr e}


mkSULog :: String -> StatusUpdate
mkSULog p = {t: "log", p: SuStr p}


mkSUSuccess :: OutAllDeets -> StatusUpdate
mkSUSuccess b = {t: "success", p: SuRes b}


mkSUWarn :: String -> StatusUpdate
mkSUWarn m = {t: "warn", p: SuStr m}


mkSUBal :: Map Address BigNumber -> StatusUpdate
mkSUBal bals = {t: "balances", p: SuBal bals}


mkSUDlgts :: Map Address Address -> StatusUpdate
mkSUDlgts dlgts = {t: "delegates", p: SuDlgt dlgts}
