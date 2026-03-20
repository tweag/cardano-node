{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Server (run) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode, getNetworkMagic)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress,
                   encodeRemoteAddress)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (configureSocket)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy)

import           Control.ResourceRegistry
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Contravariant ((>$<))
import           Data.Void (Void)
import qualified Network.Mux as Mux
import           Network.Socket (SockAddr (..))

import           Test.Consensus.PeerSimulator.Resources (PeerResources)

import           MiniProtocols (peerSimServer)


-- | Glue code for using just the bits from the Diffusion Layer that we need in
-- this context.
serve ::
  SockAddr ->
  N2N.Versions
    N2N.NodeToNodeVersion
    N2N.NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode SockAddr BL.ByteString IO Void ()) ->
  IO Void
serve sockAddr application = withIOManager \iocp -> do
  let sn = Snocket.socketSnocket iocp
      family = Snocket.addrFamily sn sockAddr
  bracket (Snocket.open sn family) (Snocket.close sn) \socket -> do
    networkMutableState <- N2N.newNetworkMutableState
    configureSocket socket (Just sockAddr)
    Snocket.bind sn socket sockAddr
    Snocket.listen sn socket
    N2N.withServer
      sn
      N2N.nullNetworkServerTracers
        { N2N.nstHandshakeTracer = show >$< stdoutTracer
        , N2N.nstErrorPolicyTracer = show >$< stdoutTracer
        }
      networkMutableState
      acceptedConnectionsLimit
      socket
      application
      nullErrorPolicies
 where
  acceptedConnectionsLimit =
    N2N.AcceptedConnectionsLimit
      { N2N.acceptedConnectionsHardLimit = maxBound
      , N2N.acceptedConnectionsSoftLimit = maxBound
      , N2N.acceptedConnectionsDelay = 0
      }

run ::
  forall blk.
  ( ConfigSupportsNode blk
  , SerialiseNodeToNodeConstraints blk
  , ShowProxy blk
  , ShowProxy (Header blk)
  , SupportedNetworkProtocolVersion blk
  ) =>
  CodecConfig blk ->
  BlockConfig blk ->
  PeerResources IO blk ->
  -- | A TMVar for the chainsync channel that we will fill in once the node connects.
  StrictTVar IO Bool ->
  -- | A TMVar for the blockfetch channel that we will fill in once the node connects.
  StrictTVar IO Bool ->
  SockAddr ->
  IO Void
run codecCfg blkCfg res csChanTMV bfChanTMV sockAddr = withRegistry \_registry ->
  serve sockAddr
    $ peerSimServer @_ @blk
      res
      csChanTMV
      bfChanTMV
      codecCfg
      encodeRemoteAddress
      decodeRemoteAddress
    $ getNetworkMagic @blk blkCfg
