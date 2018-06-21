#!/bin/bash

set -e

STATUS='\033[42;30;21m'
NC='\033[0m' # No Color

log(){
    echo -e "${STATUS}$1${NC}"
}

log "Cloning sv-light-contracts for ABIs"
mkdir -p _autogen/sv-light-contracts
cd _autogen/sv-light-contracts

if [ -e .git ]; then
    git stash && git pull origin master
else
    git clone https://github.com/secure-vote/sv-light-smart-contracts . --depth=10
fi
yarn compile-sv-light

cd ../..

rm -rf ./tmpSolDist
mkdir -p tmpSolDist

log "Coping ABIs to tmp directory for processing"

copySolFile(){
    file=$(echo "$1" | sed "s/\.abi$//g").abi
    echo "copying $file"
    cp "./_autogen/sv-light-contracts/_solDist/$file" "./tmpSolDist/$(basename "$file" .abi).json"
}

copySolFile ENSIface
copySolFile PublicResolver
copySolFile BBFarm
# copySolFile SVDelegation
copySolFile SVDelegationV0101
copySolFile FakeErc20
copySolFile SVIndex
copySolFile SVIndexBackend
copySolFile SVPayments


rm ./src/SecureVote/Contracts/* || true

log "Running web3 generator"

# pulp run -m Generator --src-path pureWeb3Gen -- --abis tmpSolDist/ --dest src --module SecureVote.Contracts
pulp build -m Generator --src-path pureWeb3Gen --to generator.js
node generator.js --abis tmpSolDist/ --dest src --module SecureVote.Contracts

log "Cleaning up"

rm generator.js
rm -r tmpSolDist

log "Done"
