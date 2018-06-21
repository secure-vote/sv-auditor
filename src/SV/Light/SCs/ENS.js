const nh = require('eth-ens-namehash')

exports.namehashImpl = function(ensName) {
    const node = nh.hash(ensName)
    console.log("calculated node", node, "from name", ensName)
    return node
}
