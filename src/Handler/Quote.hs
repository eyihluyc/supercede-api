{-#LANGUAGE OverloadedStrings #-}

module Handler.Quote where

import Import
import Language.Haskell.TH (Extension(OverloadedStrings))
import Database.Persist.Sql (toSqlKey)

postQuoteR :: Handler Value
postQuoteR = do
    quote <- (requireInsecureJsonBody :: Handler Quote)
    insertedQuote <- runDB $ insertEntity quote
    returnJson insertedQuote

getQuoteR :: Handler Value
getQuoteR = do
    layer_id <- lookupGetParam "layer_id"
    case layer_id of
        Nothing -> invalidArgs ["Missing layer_id parameter"]
        Just lid -> do
            allQuotes <- runDB $ selectList [QuoteLayer_id ==. lid] []
            returnJson allQuotes

getAllQuotes :: DB [Entity Quote]
getAllQuotes = selectList [] [Asc QuoteId]