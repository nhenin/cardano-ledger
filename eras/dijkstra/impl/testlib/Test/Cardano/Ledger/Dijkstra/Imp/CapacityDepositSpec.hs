{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Cardano.Ledger.Dijkstra.Imp.CapacityDepositSpec (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.State (UTxO (..))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Core.Utils (txInAt)
import qualified Test.Cardano.Ledger.Dijkstra.Imp.CapacityDeposit.Fixture as Fixture
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "Capacity deposit ledger integration" $ do
  it
    "rejects a signed implicit large-datum output with insufficient total ADA"
    rejectsImplicitLargeDatum
  it "accepts an exact explicit deposit with zero application ADA" acceptsZeroApplicationAda
  it "accepts an exact explicit deposit with one application lovelace" acceptsOneApplicationLovelace
  it "rejects a wrong explicit capacity deposit" rejectsWrongExplicitCapacity

rejectsImplicitLargeDatum :: DijkstraEraImp era => ImpTestM era ()
rejectsImplicitLargeDatum = do
  -- Setup
  pp <- getsPParams id
  out <- Fixture.largeImplicitOutput
  let expected = injectFailure <$> Fixture.implicitRejection pp out
  -- Exercise
  (failures, _) <-
    expectLeft
      =<< withFixup Fixture.preserveOutputAllocations (trySubmitTx (Fixture.singleOutputTx out))
  -- Verify
  failures `shouldBeExpr` expected

acceptsZeroApplicationAda :: DijkstraEraImp era => ImpTestM era ()
acceptsZeroApplicationAda = do
  -- Setup
  out <- Fixture.largeExplicitOutput (Coin 0)
  -- Exercise
  submitted <- submitTx (Fixture.singleOutputTx out)
  UTxO stored <- getUTxO
  -- Verify
  Map.lookup (txInAt 0 submitted) stored `shouldBe` Just out

acceptsOneApplicationLovelace :: DijkstraEraImp era => ImpTestM era ()
acceptsOneApplicationLovelace = do
  -- Setup
  out <- Fixture.largeExplicitOutput (Coin 1)
  -- Exercise
  submitted <- submitTx (Fixture.singleOutputTx out)
  UTxO stored <- getUTxO
  -- Verify
  Map.lookup (txInAt 0 submitted) stored `shouldBe` Just out

rejectsWrongExplicitCapacity :: DijkstraEraImp era => ImpTestM era ()
rejectsWrongExplicitCapacity = do
  -- Setup
  pp <- getsPParams id
  out <- Fixture.wrongExplicitOutput
  let expected = injectFailure (Fixture.explicitRejection pp out)
  -- Exercise
  (failures, _) <- expectLeft =<< trySubmitTx (Fixture.singleOutputTx out)
  -- Verify
  failures `shouldBeExpr` [expected]
