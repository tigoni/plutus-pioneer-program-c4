{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext(scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo(txInfoValidRange), Validator, from, mkValidatorScript, to)
import           Plutus.V1.Ledger.Interval (contains, after, before)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), (||), (&&), ($))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = ((beneficiarySigned $ beneficiary1 dat) && deadlineAt) ||  ((beneficiarySigned $ beneficiary2 dat) && deadlineReached)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiarySigned :: PubKeyHash -> Bool
        beneficiarySigned pkh = txSignedBy info pkh 

        deadlineAt :: Bool
        deadlineAt = contains (to $ deadline dat) $ txInfoValidRange info

        deadlineReached :: Bool
        deadlineReached = before (deadline dat) $ txInfoValidRange info  

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
