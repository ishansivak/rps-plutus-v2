{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RPSValidator (RPSParam (..), validator) where

import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts 
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import              PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import qualified    Prelude                   as Pr
import              Data.Aeson                  (ToJSON, FromJSON)
import              GHC.Generics                (Generic)
import qualified    Ledger                    as Lr
import              Ledger.Ada

import              RPSTypes

--This is a helper function.
{-# INLINEABLE rpsBeats #-}
rpsBeats :: RPSChoice -> RPSChoice -> Bool
rpsBeats a b = case a of                      --It outputs true if a beats b.
    Rock     ->    b == Scissors
    Paper    ->    b == Rock
    Scissors ->    b == Paper                 --When applying this function in the validator, think about the order of the datum and redeemer.

--The aim of this validator is to only be spendable when the redeemer beats the datum. E.g.: Redeemer is Rock
--It is parameterized by an arbitrary integer called game number, so each game occurs at a different script address
{-# INLINEABLE myValidator #-}
myValidator :: RPSParam -> RPSChoice -> RPSChoice -> ScriptContext -> Bool
myValidator param datum redeemer context = traceIfFalse "You played the same sign as the datum in your redeemer" (rpsBeats redeemer datum)

--As you can see, this validator doesn't implement Rock Paper Scissors correctly, try using the helper function (rpsBeats) to fix it.


typedValidator :: RPSParam -> TypedValidator RPSTypes
typedValidator faucet = go faucet where
    go = mkTypedValidatorParam @RPSTypes
        $$(PlutusTx.compile [|| myValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: RPSParam -> Validator
validator = validatorScript . typedValidator


{-
Bonus challenge:
How would you implement Rock Paper Scissors Lizard Spock?
-}