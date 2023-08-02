{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Homework2 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (unstableMakeIsData, compile)
import           PlutusTx.Prelude     (Bool (..), BuiltinData, (==), (&&), (||))
import           Prelude              (undefined, otherwise)
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- Create a validator that unlocks the funds if MyRedemeer's flags are different

--Using guards
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ redeemer _
    | flagsDiffer == True = True
    | otherwise = False
    where flagsDiffer = flag1 redeemer == False && flag2 redeemer == True 
              || flag1 redeemer == True && flag2 redeemer == False

--Using pattern matching
-- mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
-- mkValidator _ (MyRedeemer True False) _ = True
-- mkValidator _ (MyRedeemer False True) _ = True
-- mkValidator _  _ _ = False



wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal||])
