{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}

module RPSTypes where

import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api      as Oc
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

newtype RPSParam = RPSParam { gameNumber :: Integer }
    deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''RPSParam
PlutusTx.makeLift ''RPSParam

data RPSChoice = Rock | Paper | Scissors
    deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON)

instance Eq RPSChoice where
    {-# INLINABLE (==) #-}
    Rock      == Rock      = True
    Paper     == Paper     = True
    Scissors  == Scissors  = True
    _         == _         = False


PlutusTx.makeIsDataIndexed ''RPSChoice [('Rock, 0), ('Paper, 1), ('Scissors, 2)]
PlutusTx.makeLift ''RPSChoice

data RPSTypes

instance ValidatorTypes RPSTypes where
    type instance DatumType RPSTypes = RPSChoice
    type instance RedeemerType RPSTypes = RPSChoice