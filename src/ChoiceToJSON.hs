{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module ChoiceToJSON where

import            Cardano.Api             (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson              (encode)
import  qualified Data.ByteString.Lazy as LBS
import  qualified PlutusTx
import            PlutusTx.Builtins.Internal
import            Cardano.Api.Shelley     (fromPlutusData)

import            Prelude
import            RPSTypes


testChoice :: RPSChoice
testChoice = Scissors        --This is where you type the choice you want to play




writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData


main :: IO ()
main = do
      writeJSON "output/testC.json" testChoice