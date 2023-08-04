{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext(scriptContextTxInfo), Validator,
                                       mkValidatorScript, TxInfo(txInfoValidRange), to)
import           Plutus.V1.Ledger.Interval (contains, after, before, from)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (applyCode, compile, liftCode, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), (||), (&&), ($), (.))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { 
        beneficiary :: PubKeyHash
    }

unstableMakeIsData ''VestingDatum


{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = 
    beneficiarySigned beneficiary && deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiarySigned :: PubKeyHash -> Bool
        beneficiarySigned pkh = txSignedBy info pkh 

        deadlineReached :: Bool
        deadlineReached = contains (from deadline) $ txInfoValidRange info



{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
