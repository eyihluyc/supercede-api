{-#LANGUAGE OverloadedStrings #-}

module Handler.Quote where

import Import


data Version = V_2019 | V_2020
    deriving (Eq, Show)

supercedeVersionToText :: Version -> Text
supercedeVersionToText V_2019 = "2019-12-01"
supercedeVersionToText V_2020 = "2020-10-01"

parseVersion :: Text -> Maybe Version
parseVersion "2019-12-01" = Just V_2019
parseVersion "2020-10-01" = Just V_2020
parseVersion _ = Nothing

instance ToJSON Version where
    toJSON x = String (supercedeVersionToText x)


postQuoteR :: Handler Value
postQuoteR = handleVersion postQuote

getQuoteR :: Handler Value
getQuoteR = handleVersion getQuote

handleVersion :: Handler Value -> Handler Value
handleVersion handler = do
    supercedeVersionMaybe <- lookupHeader "Supercede-Version"
    case supercedeVersionMaybe of
        Nothing -> supercedeVersionError
        Just sv -> case parseVersion (decodeUtf8 sv) of
            Nothing -> supercedeVersionError
            Just _ -> handler

supercedeVersionError :: Handler Value
supercedeVersionError = do
    sendStatusJSON 
        status400 
        (object [ ("error", "Invalid or missing Supercede-Version header")
                , "available_versions" .= [V_2019, V_2020]])

postQuote :: Handler Value
postQuote = do
    quote <- (requireInsecureJsonBody :: Handler Quote)
    insertedQuote <- runDB $ insertEntity quote
    sendStatusJSON created201 insertedQuote

getQuote :: Handler Value
getQuote = do
    layer_id <- lookupGetParam "layer_id"
    case layer_id of
        Nothing -> invalidArgs ["Missing layer_id parameter"]
        Just lid -> do
            allQuotes <- runDB $ selectList [QuoteLayer_id ==. lid] []
            returnJson allQuotes

getAllQuotes :: DB [Entity Quote]
getAllQuotes = selectList [] [Asc QuoteId]