{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.QuoteSpec (spec) where

import TestImport
import Data.Aeson
import Network.Wai.Test (simpleBody)

spec :: Spec
spec = withApp $ do

    let quoteName = "my quote name"
        layer_id = "1"
        stamp_name = "my stamp name"
        body = object [ "name" .= quoteName
                        , "layer_id" .= layer_id
                        , "stamp_name" .= stamp_name]
        encoded = encode body

    let versionErrorMsg = encode $ object [ ("error", "Invalid or missing Supercede-Version header") 
                                          , "available_versions" .= ["2019-12-01" :: Text, "2020-10-01" :: Text]]

    describe "valid POST request" $ do
        it "gives a 200" $ do

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Supercede-Version", "2020-10-01")
            statusIs 201

            quotes <- runDB $ selectList [QuoteName ==. quoteName] []
            Entity _id quote <-
                case quotes of
                    [ent] -> pure ent
                    _ -> error "needed 1 entity"
            assertEq "Should have " quote (Quote quoteName layer_id stamp_name Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

    describe "valid POST request with \"reinsurance_premium\" field" $ do
        it "gives a 200" $ do

            let quoteName = "my quote name 2"
                layer_id = "2"
                stamp_name = "my stamp name 2"
                reinsurance_premium = Money 200 "USD"
                body = object [ "name" .= quoteName
                              , "layer_id" .= layer_id
                              , "stamp_name" .= stamp_name
                              , "reinsurance_premium" .= reinsurance_premium]
                encoded = encode body

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Supercede-Version", "2020-10-01")
            statusIs 201

            quotes <- runDB $ selectList [QuoteName ==. quoteName] []
            Entity _id quote <-
                case quotes of
                    [ent] -> pure ent
                    _ -> error "needed 1 entity"
            assertEq "Should have " quote (Quote quoteName layer_id stamp_name Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just reinsurance_premium) Nothing Nothing)

    describe "invalid requests" $ do
        it "400s when the JSON body is invalid" $ do

            let body = object [ "foo" .= ("My quoteName" :: Value) ]

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Supercede-Version", "2020-10-01")
            statusIs 400

    describe "quotes table" $ do
        it "gets a row after a valid POST request" $ do
            quotesBefore <- runDB $ selectList ([] :: [Filter Quote]) []
            let rowsBefore = length quotesBefore

            request $ do
                setMethod "POST"
                setUrl QuoteR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Supercede-Version", "2020-10-01")
            statusIs 201

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
                addRequestHeader ("Supercede-Version", "2020-10-01")
            statusIs 200

            responseMaybe <- getResponse
            case responseMaybe of
                Just response -> do
                    let quotesInResponse = decode (simpleBody response) :: Maybe [Quote]
                    case quotesInResponse of
                        Just quotes' -> assertEq "Number of quotes in response matches database" quotesInDB (length quotes')
                        Nothing      -> error "Failed to decode response body"
                Nothing -> error "No response received"

        it "returns an error response if Supercede-Version is not provided" $ do
            request $ do
                setMethod "GET"
                setUrl QuoteR
                addGetParam "layer_id" "1"
                addRequestHeader ("Content-Type", "application/json")
            statusIs 400

            responseMaybe <- getResponse
            case responseMaybe of
                Just response -> do
                    assertEq "Should send available versions" versionErrorMsg (simpleBody response)
                Nothing -> error "No response received"
        
        it "returns an error response if the provided Supercede-Version is not supported" $ do
            request $ do
                setMethod "GET"
                setUrl QuoteR
                addGetParam "layer_id" "1"
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Supercede-Version", "2024-04-18")
            statusIs 400

            responseMaybe <- getResponse
            case responseMaybe of
                Just response -> do
                    assertEq "Should send available versions" versionErrorMsg (simpleBody response)
                Nothing -> error "No response received"