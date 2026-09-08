{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test instances for the Dijkstra-owned output. Their dependencies on existing
-- testlib instances prevent placing them in the production type's module.
module Test.Cardano.Ledger.Dijkstra.TxOut () where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Dijkstra.TxOut
import Cardano.Ledger.Plutus (Datum)
import qualified Data.TreeDiff.OMap as OMap
import Lens.Micro
import Test.Cardano.Ledger.Common hiding (output)
import Test.Cardano.Ledger.Dijkstra.TxOut.Value ()
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.TreeDiff (Expr (..))

instance
  ( DijkstraEraTxOut era
  , TxOut era ~ DijkstraTxOut era
  , Arbitrary (Script era)
  , Arbitrary (Datum era)
  , Arbitrary (Value era)
  ) =>
  Arbitrary (DijkstraTxOut era)
  where
  arbitrary = do
    output <- mkBasicTxOut <$> arbitrary <*> arbitrary
    datum <- arbitrary
    script <- arbitrary
    explicit <- arbitrary
    value <- scale (`div` 15) arbitrary
    pure $
      ( if explicit
          then output & outputValueTxOutL .~ value & capacityDepositFormTxOutL .~ ExplicitCapacityDeposit
          else output
      )
        & datumTxOutL .~ datum
        & referenceScriptTxOutL .~ script

instance
  ( DijkstraEraTxOut era
  , TxOut era ~ DijkstraTxOut era
  , ToExpr (Script era)
  , ToExpr (Datum era)
  ) =>
  ToExpr (DijkstraTxOut era)
  where
  toExpr output =
    Rec "DijkstraTxOut" $
      OMap.fromList
        [ ("address", toExpr $ output ^. addrTxOutL)
        , ("outputValue", toExpr $ output ^. outputValueTxOutL)
        , ("capacityDepositForm", toExpr . show $ output ^. capacityDepositFormTxOutL)
        , ("datum", toExpr $ output ^. datumTxOutL)
        , ("referenceScript", toExpr $ output ^. referenceScriptTxOutL)
        ]
