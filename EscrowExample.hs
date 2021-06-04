{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-- A escrow contract where someone can exchange an NFT for
-- another specified NFT, or for a predefined amount of ADA.
-- If someone wants to get this token, can choose between paying 
-- with the specified NFT or with the price in ADA.
import           Control.Monad             (void)
import Data.Text
import qualified Data.Map             as Map
import qualified Data.ByteString.Char8     as C
import           Language.Plutus.Contract
import Ledger.AddressMap(UtxoMap)
import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude hiding (pure, (<$>))
import           Ledger               hiding (singleton)
import qualified Ledger.Value         as Value
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Ledger.Ada           as Ada
import           Playground.Contract
import qualified Prelude
import Prelude ((<$>))
import           Text.Printf          (printf)
------------------------------------------------------------
data TokenInfo = TokenInfo { currency  :: !CurrencySymbol
                           , tokenName :: !TokenName
                           }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq TokenInfo where
    {-# INLINABLE (==) #-}
    t1 == t2 = (currency t1  == currency t2) &&
               (tokenName t1 == tokenName t2)

PlutusTx.makeIsData ''TokenInfo
PlutusTx.makeLift ''TokenInfo

data SellInfo = SellInfo { seller          :: !PubKeyHash
                         -- token on sale
                         , sellNFT    :: !TokenInfo
                         -- token to receive
                         , buyNFT     :: !TokenInfo
                         -- ADA to receive
                         , priceInADA      :: !Integer
                         }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsData ''SellInfo
PlutusTx.makeLift ''SellInfo

data BuyInfo = BuyWithNFT !TokenInfo
             | BuyWithADA !Integer 
    deriving Show

PlutusTx.makeIsData ''BuyInfo
PlutusTx.makeLift ''BuyInfo

data SellParams = SellParams { spSellNFT    :: !TokenName
                             -- token to receive
                             , spBuyNFT     :: !TokenName
                             -- ADA to receive
                             , spPriceInADA :: !Integer
                             }
    deriving (Generic, ToJSON, FromJSON, ToSchema)

data BuyWithNFTParams = BuyWithNFTParams { bpSellNFT  :: !TokenName
                                         , bpBuyNFT   :: !TokenName
                                         }
    deriving (Generic, ToJSON, FromJSON, ToSchema)

data BuyWithADAParams = BuyWithADAParams { bpWASellNFT  :: !TokenName
                                         , bpAmountADA  :: !Integer
                                         }
    deriving (Generic, ToJSON, FromJSON, ToSchema)

type EscrowSchema =
    BlockchainActions
        .\/ Endpoint "sell" SellParams
        .\/ Endpoint "buyWithNFT" BuyWithNFTParams
        .\/ Endpoint "buyWithADA" BuyWithADAParams
data Escrow

instance Scripts.ScriptType Escrow where
    type instance RedeemerType Escrow = BuyInfo
    type instance DatumType Escrow = SellInfo

escrowInstance :: Scripts.ScriptInstance Escrow
escrowInstance = Scripts.validator @Escrow
    $$(PlutusTx.compile [|| validatePrice ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SellInfo @BuyInfo

-- | The validation function (Datum -> Redeemer -> ValidatorCtx -> Bool)
validatePrice :: SellInfo -> BuyInfo -> ValidatorCtx -> Bool
validatePrice SellInfo{..} (BuyWithNFT t) _      = buyNFT == t
validatePrice SellInfo{..} (BuyWithADA amount) _ = priceInADA == amount

-- | The validator script of the escrow.
escrowValidator :: Validator
escrowValidator = Scripts.validatorScript escrowInstance

-- | The address of the escrow (the hash of its validator script)
escrowAddress :: Address
escrowAddress = Ledger.scriptAddress escrowValidator

makeTokenInfo :: TokenName -> Contract EscrowSchema Text TokenInfo
makeTokenInfo "S"  = return $ TokenInfo "73" "S"
makeTokenInfo "F" = return $ TokenInfo "66" "F"
makeTokenInfo "T"  = return $ TokenInfo "74" "T"
makeTokenInfo _ = throwError "unrecognized token name"

-- | The "sell" contract endpoint
sell :: Contract EscrowSchema Text ()
sell = do 
    SellParams{..} <- endpoint @"sell" @SellParams
    sellToken <- makeTokenInfo spSellNFT
    buyToken <- makeTokenInfo spBuyNFT
    pkh <- pubKeyHash <$> ownPubKey
    let sInfo = SellInfo { seller = pkh
                         , sellNFT = sellToken
                         , buyNFT = buyToken
                         , priceInADA = spPriceInADA
                         }
        v = Value.singleton (currency sellToken) (tokenName sellToken) 1 
        tx = Constraints.mustPayToTheScript sInfo v
    void (submitTxConstraints escrowInstance tx)

-- | The "buyWithNFT" contract endpoint
buyWithNFT :: Contract EscrowSchema Text ()
buyWithNFT = do
    BuyWithNFTParams{..} <- endpoint @"buyWithNFT" @BuyWithNFTParams
    sellToken <- makeTokenInfo bpSellNFT
    buyToken <- makeTokenInfo bpBuyNFT
    unspentOutputs <- filterByNFT sellToken <$> (utxoAt escrowAddress)
    SellInfo{..} <- getDatumFromUtxo unspentOutputs sellToken
    let bInfo = BuyWithNFT buyToken
        v = Value.singleton (currency buyToken) (tokenName buyToken) 1
        tx  = collectFromScript unspentOutputs bInfo <>
              Constraints.mustPayToPubKey seller v
    void (submitTxConstraintsSpending escrowInstance unspentOutputs tx)

-- | The "buyWithADA" contract endpoint
buyWithADA :: Contract EscrowSchema Text ()
buyWithADA = do
    BuyWithADAParams{..} <- endpoint @"buyWithADA" @BuyWithADAParams
    sellToken <- makeTokenInfo bpWASellNFT
    unspentOutputs <- filterByNFT sellToken <$> (utxoAt escrowAddress)
    s@SellInfo{..} <- getDatumFromUtxo unspentOutputs sellToken
    logInfo @String $ printf "found sell %s" (show s)
    logInfo @String $ printf "attempting to pay %s" (show bpAmountADA)

    let bInfo = BuyWithADA bpAmountADA
        v =  Ada.lovelaceValueOf bpAmountADA
        tx  = collectFromScript unspentOutputs bInfo <>
              Constraints.mustPayToPubKey seller v
    void (submitTxConstraintsSpending escrowInstance unspentOutputs tx)


filterByNFT ::  TokenInfo -> UtxoMap -> UtxoMap
filterByNFT tinf@TokenInfo{..} = 
    Map.filter (\o -> Value.valueOf (txOutValue $ txOutTxOut o) currency tokenName == 1)

getDatumFromUtxo :: UtxoMap -> TokenInfo -> Contract EscrowSchema Text SellInfo
getDatumFromUtxo utxos tinf@TokenInfo{..} = 
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (txOutValue $ txOutTxOut o) currency tokenName == 1
             ] in
    case xs of
        [(oref, o)] -> case txOutType $ txOutTxOut o of
            PayToPubKey   -> throwError "unexpected out type"
            PayToScript h -> case Map.lookup h $ txData $ txOutTxTx o of
                Nothing        -> throwError "datum not found"
                Just (Datum e) -> case PlutusTx.fromData e of
                    Nothing -> throwError "datum has wrong type"
                    Just d@SellInfo{..}
                        | sellNFT == tinf -> return d
                        | otherwise       -> throwError "escrow token missmatch"
        _           -> throwError "auction utxo not found"

escrow :: Contract EscrowSchema Text ()
escrow = sell `select` buyWithNFT `select` buyWithADA

endpoints :: Contract EscrowSchema Text ()
endpoints = escrow

mkSchemaDefinitions ''EscrowSchema

sToken :: KnownCurrency
sToken = KnownCurrency (ValidatorHash "s") "Token" (TokenName "S" :| [])

fToken :: KnownCurrency
fToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "F" :| [])

tToken :: KnownCurrency
tToken = KnownCurrency (ValidatorHash "t") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['sToken,'fToken,'tToken]
