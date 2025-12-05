#!/usr/bin/env bash

# This is a script which helps connect the test-runner to a cardano-node. The
# exact interleaving of processes here is nontrivial:
#
# 1. Start test-runner
# 2. Start cardano-node
# 3. After test-runner closes, kill cardano-node
#
# In addition, test-runner must know about a socket-path that will be created
# by cardano-node, and cardano-node needs a topology file created by
# test-runner.

export LC_ALL=C.UTF-8
cabal run cardano-node:conformance-test-runner -- \
  --topology-file=/tmp/topology.file \
  --socket-path=/tmp/cardano.socket \
  --port=6000 \
  unused_mandatory_argument &
TEST_PID=$!

# test-runner might require a build, so we can't necessarily start cardano-node
# immediately. Therefore, we wait until test-runner starts listening on port
# 6000.
while ! nc -z localhost 6000; do
    sleep 0.5
done

# Now that the test harness is up, we can start the NUT, which will connect to
# test-runner via the generated topology file.
cabal run cardano-node:cardano-node -- run \
    --topology=/tmp/topology.file \
    --socket-path=/tmp/cardano.socket 1>/dev/null &
NUT_PID=$!

# Wait for test-runner to exit, and capture its exit code.
wait "$TEST_PID"
NUT_RESULT=$?

# Once it has finished, gracefully request that the NUT exit.
if [ -n "${NUT_PID-}" ]; then
    kill "$NUT_PID" 2>/dev/null
    wait "$NUT_PID" 2>/dev/null
fi

# Exit with the same code that test-runner gave.
exit "$NUT_RESULT"
