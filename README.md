# SV Auditor

## Overview

The SV Auditor is provided with:

* `ethNodes :: StrMap String`: a map of networkId -> nodeUrl for any networks required
* `indexEns :: String`: the ENS address of the index
* `ensDetails :: StrMap String`: the Eth addresses of the ENS register contract for all relevant networks
* `ballotId :: String`: the BallotID of the ballot we'd like to audit (base10 string)
* `startingNetwork :: String`: the network ID we're starting on (note: we might need to cross into other networks, so best to fully populate )

any also

* a function that receives updates from the auditor

`SvAudit.main(args)(updateF)()`

The SV Auditor works by:

* talking to the index to find the location of the BBFarm
* getting the details of the vote from the BBFarm
* loading all ballots, balances, and delegations at their relevant times
* performing the count
* returning results through `updateF`
