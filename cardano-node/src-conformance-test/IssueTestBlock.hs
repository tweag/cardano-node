{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IssueTestBlock () where

import           Cardano.Crypto.Hash as Hash
import           Cardano.Crypto.KES as KES
import           Cardano.Crypto.VRF.Class (deriveVerKeyVRF)
import           Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq)
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Keys hiding (hashVerKeyVRF)
import           Cardano.Ledger.Shelley.API hiding (hashVerKeyVRF)
import           Cardano.Ledger.Shelley.Core
import           Cardano.Node.Protocol.Shelley
import           Cardano.Node.Types hiding (GenesisHash)
import           Cardano.Protocol.Crypto
import           Cardano.Protocol.TPraos.BHeader
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock, pattern BlockConway)
import           Ouroboros.Consensus.Protocol.Praos.Common (PraosCanBeLeader (..))
import           Ouroboros.Consensus.Protocol.Praos.Header
import           Ouroboros.Consensus.Protocol.Praos.VRF (mkInputVRF)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock (..), ShelleyHash (..))
import           Ouroboros.Consensus.Shelley.Node.Common (ShelleyLeaderCredentials (..))
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()

import           Control.Monad.Trans.Except
import           System.FilePath ((</>))

import           Test.Cardano.Ledger.Shelley.Utils (mkCertifiedVRF)

import           Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock (..))


-- | Data required for issuing cardano blocks.
data CardanoBlockCtx = CardanoBlockCtx
  { cbcCredentials :: ShelleyLeaderCredentials StandardCrypto
    -- ^ Necessary keys and certificates.
  , cbcEpochNonce  :: Nonce
    -- ^ The nonce, required to pass some VRF checks.
  }

instance IssueTestBlock (CardanoBlock StandardCrypto) where
  type TestBlockContext (CardanoBlock StandardCrypto) = CardanoBlockCtx
  getTestBlockContext _ = do
    -- TODO(isovector): Use real command line configuration for these paths.
    let prefix = "./configuration/ctc"
        hardcodedPaths = ProtocolFilepaths
          { byronCertFile        = Nothing
          , byronKeyFile         = Nothing
          , shelleyKESFile       = Just $ prefix </> "delegate-keys/delegate1/kes.skey"
          , shelleyVRFFile       = Just $ prefix </> "delegate-keys/delegate1/vrf.skey"
          , shelleyCertFile      = Just $ prefix </> "delegate-keys/delegate1/opcert.cert"
          , shelleyBulkCredsFile = Nothing
          }
    Right (creds:_) <- runExceptT $ readLeaderCredentials $ Just hardcodedPaths
    -- Compute the initial epoch nonce the same way cardano-node does:
    -- it's the hash of the shelley genesis file.
    Right (_, genesisHash) <- runExceptT $
      readGenesis (GenesisFile $ prefix </> "shelley-genesis.json") Nothing
    pure $ CardanoBlockCtx
      { cbcCredentials = creds
      , cbcEpochNonce = genesisHashToPraosNonce genesisHash
      }
  issueFirstBlock ctx fork slot =
    makeCardanoBlock ctx (Just fork) 0 slot Nothing
  issueSuccessorBlock ctx fork slot (BlockConway (ShelleyBlock
      (Block hdr _)
      _)) =
    makeCardanoBlock ctx fork
      (hbBlockNo (headerBody hdr) + 1)
      (hbSlotNo  (headerBody hdr) + slot + 1)
      (Just $ HashHeader $ headerHash hdr)
  issueSuccessorBlock _ _ _ _ =
    error "issueSuccessorBlock: impossible, since all blocks are guaranteed to be BlockConway"


-- | Construct a fake 'CardanoBlock' with all of its crypto intact.
makeCardanoBlock
  :: CardanoBlockCtx
  -> Maybe Int
  -- ^ Fork number
  -> BlockNo
  -> SlotNo
  -> Maybe HashHeader
  -> CardanoBlock StandardCrypto
makeCardanoBlock ctx fork blockNo slot mhash = BlockConway $
  let blk@(Block (Header bhb _) _) = conwayLedgerBlock ctx fork slot blockNo mhash
    in ShelleyBlock blk $ ShelleyHash $ castHash $ hbBodyHash bhb


-- | Construct a made-up (but believable) cardano block for the Conway era.
conwayLedgerBlock
  :: CardanoBlockCtx
  -> Maybe Int
  -- ^ Fork number, encoded in the minor protocol version to distinguish block hashes.
  -> SlotNo
  -> BlockNo
  -> Maybe HashHeader
  -- ^ The parent hash, if there is one.
  -> Block (Header StandardCrypto) ConwayEra
conwayLedgerBlock CardanoBlockCtx{cbcCredentials, cbcEpochNonce} fork slot blockNo prev =
    Block blockHeader blockBody
  where
    PraosCanBeLeader
        { praosCanBeLeaderSignKeyVRF
        , praosCanBeLeaderColdVerKey
        , praosCanBeLeaderOpCert
        } = shelleyLeaderCredentialsCanBeLeader cbcCredentials

    blockHeader :: Header StandardCrypto
    blockHeader =
        Header blockHeaderBody $
          unsoundPureSignedKES () 0 blockHeaderBody $
            shelleyLeaderCredentialsInitSignKey cbcCredentials

    blockHeaderBody :: HeaderBody StandardCrypto
    blockHeaderBody =
      HeaderBody
        { hbBlockNo = blockNo
        , hbSlotNo = slot
        , hbPrev = maybe GenesisHash BlockHash prev
        , hbVk = coerceKeyRole praosCanBeLeaderColdVerKey
        , hbVrfVk = deriveVerKeyVRF praosCanBeLeaderSignKeyVRF
        , hbVrfRes = mkCertifiedVRF (mkInputVRF slot cbcEpochNonce) praosCanBeLeaderSignKeyVRF
        , hbBodySize = fromIntegral $ bBodySize protVer blockBody
        , hbBodyHash = hashTxSeq blockBody
        , hbOCert = praosCanBeLeaderOpCert
        , hbProtVer = protVer
        }

    blockBody :: AlonzoTxSeq ConwayEra
    blockBody = toTxSeq mempty

    -- The 'IssueTestBlock' interface requires that the first block of each
    -- adversarial fork has a distinct hash from the corresponding trunk block
    -- at the same slot. Only the first block of each fork carries
    -- a non-Nothing fork number; all subsequent blocks in that fork pass
    -- Nothing (see 'mkTestBlocks' in GenChains.hs).
    --
    -- 'CardanoBlock' has no dedicated "fork" field, so we encode the fork
    -- number in 'hbProtVer' minor version. This is included verbatim in the
    -- CBOR serialisation of 'HeaderBody' (see 'encCBOR' instance in
    -- Praos.Header), and 'headerHash' hashes that serialisation, so different
    -- minor versions produce different block hashes. Only the major version is
    -- checked by the ledger (for hard-fork transitions), so this does not
    -- cause any validation failures.
    protVer :: ProtVer
    protVer = ProtVer (eraProtVerLow @ConwayEra) (maybe 0 fromIntegral fork)



