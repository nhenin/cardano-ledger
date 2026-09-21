{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxOut (upgradeShelleyTxOut) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.TxOut (
  ShelleyTxOut (..),
  addrEitherShelleyTxOutL,
  valueEitherShelleyTxOutL,
 )
import Data.Coerce (coerce)
import Lens.Micro ((^.))

instance EraTxOut AllegraEra where
  type TxOut AllegraEra = ShelleyTxOut AllegraEra

  mkBasicTxOut = ShelleyTxOut

  upgradeTxOut _ = upgradeShelleyTxOut

  addrEitherTxOutL = addrEitherShelleyTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherShelleyTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinTxOut pp _txOut = pp ^. ppMinUTxOValueL

-- | Upgrade a Shelley output without changing its value.
upgradeShelleyTxOut :: ShelleyTxOut ShelleyEra -> ShelleyTxOut AllegraEra
upgradeShelleyTxOut (TxOutCompact addr cfval) = TxOutCompact (coerce addr) cfval
