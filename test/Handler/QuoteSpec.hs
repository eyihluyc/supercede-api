{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.QuoteSpec (spec) where

import TestImport
import Data.Aeson
import Network.Wai.Test (simpleBody)

spec :: Spec
spec = withApp $ do

    describe "valid request" $ do
        it "gives a 200" $ do

            let quoteName = "quote1" :: Text
                layer_id = "1"
                body = object [ "name" .= quoteName
                              , "layer_id" .= layer_id ]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            statusIs 200

            quotes <- runDB $ selectList [QuoteName ==. quoteName] []
            Entity _id quote <-
                case quotes of
                    [ent] -> pure ent
                    _ -> error "needed 1 entity"
            assertEq "Should have " quote (Quote quoteName layer_id)

    describe "invalid requests" $ do
        it "400s when the JSON body is invalid" $ do

            let body = object [ "foo" .= ("My quoteName" :: Value) ]

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

    describe "quotes table" $ do
        it "gets a row after a valid POST request" $ do
            quotesBefore <- runDB $ selectList ([] :: [Filter Quote]) []
            let rowsBefore = length quotesBefore

            let quoteName = "quote1" :: Text
                body = object [ "name" .= quoteName
                              , "layer_id" .= ("1" :: Text)]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")

            quotesAfter <- runDB $ selectList ([] :: [Filter Quote]) []
            let rowsAfter = length quotesAfter

            assertEq "Quotes table is incremented" (rowsBefore + 1) rowsAfter

    describe "GET quotes" $ do
        it "returns the same number of quotes as in the database" $ do
            quotesInDB <- runDB $ count ([] :: [Filter Quote])

            request $ do
                setMethod "GET"
                setUrl QuoteR
                addGetParam "layer_id" "1"
                addRequestHeader ("Content-Type", "application/json")
            statusIs 200

            responseMaybe <- getResponse
            case responseMaybe of
                Just response -> do
                    let quotesInResponse = decode (simpleBody response) :: Maybe [Quote]
                    case quotesInResponse of
                        Just quotes' -> assertEq "Number of quotes in response matches database" quotesInDB (length quotes')
                        Nothing      -> error "Failed to decode response body"
                Nothing -> error "No response received"
