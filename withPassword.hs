{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    , password :: ByteString
    } deriving (Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MyRedeemer


data GameDatum = GameDatum
    { amount :: Value
     ,setPassword :: ByteString
    } deriving Show

PlutusTx.unstableMakeIsData ''GameDatum

{-# INLINABLE mkValidator #-}
mkValidator :: GameDatum ->MyRedeemer -> ScriptContext -> Bool
mkValidator dat (MyRedeemer b c p) ctx = traceIfFalse "No Redeemer" True

data Game
instance Scripts.ScriptType Game where
    type instance DatumType Game = GameDatum
    type instance RedeemerType Game =  MyRedeemer

inst :: Scripts.ScriptInstance Game
inst = Scripts.validator @Game
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @MyRedeemer
validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
data Params = Params
    {pAmount :: Value
    ,pPassword :: ByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
give :: (HasBlockchainActions s, AsContractError e) => Params -> Contract w s e ()
give params = do
    let dat = GameDatum
                {  amount = (pAmount params),
                   setPassword = (pPassword params)
                }
        tx  = mustPayToTheScript dat $  (pAmount params)
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => MyRedeemer -> Contract w s e ()
grab bools = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData bools | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx


type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" Params
        .\/ Endpoint "grab"  MyRedeemer

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab' ) >> endpoints
  where
    give' = endpoint @"give" >>=  give
    grab' = endpoint @"grab" >>=  grab


mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []