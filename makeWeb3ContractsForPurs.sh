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
    git pull origin master
else
    git clone https://github.com/secure-vote/sv-light-smart-contracts . --depth=10
fi

cd ../..

mkdir -p tmpSolDist
rm ./tmpSolDist/* || true

log "Coping ABIs to tmp directory for processing"
for file in ./_autogen/sv-light-contracts/_solDist/*.abi; do
    if [[ $(basename "$file") =~ ^[A-Z] ]]; then
        echo "copying $file"
        cp "$file" "./tmpSolDist/$(basename "$file" .abi).json"
    else
        echo "skipping $file"
    fi
done

cp ./_autogen/sv-light-contracts/archive/swm-v2/SVLightBallotBox.abi ./tmpSolDist/BallotBoxVersion2.json

# ls ./tmpSolDist

rm ./src/SecureVote/Contracts/* || true

log "Running web3 generator"

# pulp run -m Generator --src-path pureWeb3Gen -- --abis tmpSolDist/ --dest src --module SecureVote.Contracts
pulp build -m Generator --src-path pureWeb3Gen --to generator.js
node generator.js --abis tmpSolDist/ --dest src --module SecureVote.Contracts

log "Cleaning up"

rm generator.js
rm -r tmpSolDist

log "Done"
