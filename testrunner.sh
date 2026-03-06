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

# Setup -----------------------------------------------------------------------
# -----------------------------------------------------------------------------

export LC_ALL=C.UTF-8

DBDIR="$(mktemp -d /tmp/conformance-db.XXXXXX)"

reap_process() {
  local pid="${1-}"

  # If the PID is empty, there's nothing to reap.
  [ -n "$pid" ] || return 0

  # If this PID is in use, attempt to kill it and wait for it to exit.
  if kill -0 "$pid" 2>/dev/null; then
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi

  # notes:
  #   - `${1-}` avoids an unbound variable error if `$1` is not set
  #   - `kill -0` checks if a process is still running without sending a signal
  #   - `2>/dev/null` suppresses error messages if a process is not running
  #   - `|| true` prevents the script from exiting if `kill` or `wait` fails
}

cleanup() {
  # kill any remaining processes, just in case
  reap_process "$TEST_PID"
  reap_process "$NUT_PID"
  # clean up the temporary database directory
  rm -rf "$DBDIR"
}

trap cleanup EXIT INT TERM

# Run -------------------------------------------------------------------------
# -----------------------------------------------------------------------------

echo "Starting test-runner and cardano-node..."
cabal run cardano-node:conformance-test-runner \
  --ghc-options="-Wwarn" -- \
  --topology-file=/tmp/topology.file \
  --socket-path=/tmp/cardano.socket \
  --port=6000 \
  unused_mandatory_argument &
TEST_PID=$!

# test-runner might require a build, so we can't necessarily start cardano-node
# immediately. Therefore, we wait until test-runner starts listening on port
# 6000. Also check that the topology file has been created.
while ! nc -z localhost 6000 || [ ! -s /tmp/topology.file ]; do
  echo "Waiting for test-runner to start and create topology file..."
  sleep 0.5
done

# Now that the test harness is up, we can start the NUT, which will connect to
# test-runner via the generated topology file.
cabal run cardano-node:cardano-node \
    --ghc-options="-Wwarn" +RTS -V0 -RTS -- run \
    --topology=/tmp/topology.file \
    --database-path="$DBDIR" \
    --socket-path=/tmp/cardano.socket &
NUT_PID=$!

# Wait for test-runner to exit, and capture its exit code.
echo "Waiting for test-runner to finish..."
wait "$TEST_PID"
TEST_RESULT=$?

# Once it has finished, gracefully request that the NUT exit.
reap_process "$NUT_PID"

# Exit with the same code that test-runner gave.
echo "Test runner exited with code $TEST_RESULT"
exit "$TEST_RESULT"
