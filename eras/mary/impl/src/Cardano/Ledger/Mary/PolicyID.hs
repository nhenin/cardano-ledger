{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A native-asset policy identifier in the Ledger model. The identifier is
-- the hash of the policy script, not a sentinel for Ada.
module Cardano.Ledger.Mary.PolicyID (
  PolicyID (..),
) where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Hashes (ScriptHash)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

-- | Policy ID
newtype PolicyID = PolicyID {policyID :: ScriptHash}
  deriving
    ( Show
    , Eq
    , Ord
    , Generic
    , NoThunks
    , NFData
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , ToJSONKey
    , FromJSONKey
    )
