
run_echo () {
    echo "export CARDANO_NODE=$(cabal list-bin cardano-node)"
    echo "export CARDANO_CLI=$(cabal list-bin cardano-cli)"
    echo "export CARDANO_TESTNET=$(cabal list-bin cardano-testnet)"
}

cabal build cardano-node && cabal build cardano-cli && cabal build cardano-testnet && run_echo
