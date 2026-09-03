{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Native-asset names in the Ledger model. Names are arbitrary bytes, not
-- necessarily text, and identify an asset within its policy. The CBOR decoder
-- enforces the 32-byte limit; the raw constructor does not validate it.
module Cardano.Ledger.Mary.AssetName (
  AssetName (..),
  assetNameToTextAsHex,
) where

import Cardano.Ledger.Binary (DecCBOR (..), DecoderError (..), EncCBOR, cborError)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import NoThunks.Class (NoThunks)

-- | Asset Name
newtype AssetName = AssetName {assetNameBytes :: SBS.ShortByteString}
  deriving newtype
    ( Eq
    , EncCBOR
    , Ord
    , NoThunks
    , NFData
    )

instance Show AssetName where
  show = show . assetNameToBytesAsHex

assetNameToBytesAsHex :: AssetName -> BS.ByteString
assetNameToBytesAsHex = BS16.encode . SBS.fromShort . assetNameBytes

assetNameToTextAsHex :: AssetName -> Text
assetNameToTextAsHex = decodeLatin1 . assetNameToBytesAsHex

instance DecCBOR AssetName where
  decCBOR = do
    an <- decCBOR
    if SBS.length an > 32
      then
        cborError $
          DecoderErrorCustom "asset name exceeds 32 bytes:" $
            assetNameToTextAsHex $
              AssetName an
      else pure $ AssetName an

instance ToJSON AssetName where
  toJSON = Aeson.String . assetNameToTextAsHex

instance ToJSONKey AssetName where
  toJSONKey = toJSONKeyText assetNameToTextAsHex
