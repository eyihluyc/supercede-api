module Handler.Quote where

import Import

postQuoteR :: Handler Value
postQuoteR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    quote <- (requireInsecureJsonBody :: Handler Quote)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.

    insertedQuote <- runDB $ insertEntity quote
    returnJson insertedQuote

getQuoteR :: Handler Value
getQuoteR = do
    allQuotes <- runDB getAllQuotes
    returnJson allQuotes


getAllQuotes :: DB [Entity Quote]
getAllQuotes = selectList [] [Asc QuoteId]