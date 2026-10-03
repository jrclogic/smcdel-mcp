{-# LANGUAGE OverloadedStrings #-}
module ServerSpec (spec) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai (requestMethod, requestHeaders)
import Network.Wai.Test
import Test.Hspec

import Helpers
import SMCDEL.MCP.Protocol
import SMCDEL.MCP.Server

spec :: Spec
spec = beforeAll (newServerConfig 30 >>= mcpApp) $ do
  describe "POST /mcp" $ do
    it "answers a request with 200 and a JSON-RPC response" $ \app -> do
      res <- runSession (postJson [] (rpc "initialize" (object [ "protocolVersion" .= defaultProtocolVersion ]))) app
      simpleStatus res `shouldBe` status200
      lookup hContentType (simpleHeaders res) `shouldSatisfy` maybe False (BS.isPrefixOf "application/json")
      let reply = decode (simpleBody res) :: Maybe Value
      (asText . path ["result","protocolVersion"] =<< reply) `shouldBe` Just defaultProtocolVersion
    it "answers a notification with 202 and an empty body" $ \app -> do
      res <- runSession (postJson [] (object [ "jsonrpc" .= ("2.0" :: T.Text), "method" .= ("notifications/initialized" :: T.Text) ])) app
      simpleStatus res `shouldBe` status202
      simpleBody res `shouldBe` ""
    it "runs a tool end to end" $ \app -> do
      res <- runSession (postJson [] (rpc "tools/call" (object
        [ "name" .= ("smcdel_valid" :: T.Text)
        , "arguments" .= object [ "structure" .= minimal, "formula" .= ("[! 1] alice knows that (bob knows that 1)" :: T.Text) ] ]))) app
      simpleStatus res `shouldBe` status200
      let reply = decode (simpleBody res) :: Maybe Value
      (asBool . path ["result","structuredContent","holds"] =<< reply) `shouldBe` Just True
    it "answers invalid JSON with a parse error response" $ \app -> do
      res <- runSession (postRaw [] "{") app
      simpleStatus res `shouldBe` status200
      let reply = decode (simpleBody res) :: Maybe Value
      (asInt . path ["error","code"] =<< reply) `shouldBe` Just (-32700)
    it "accepts local origins" $ \app -> do
      res <- runSession (postJson [("Origin", "http://localhost:3000")] (rpc "ping" (object []))) app
      simpleStatus res `shouldBe` status200
    it "rejects foreign origins" $ \app -> do
      res <- runSession (postJson [("Origin", "http://evil.example.com")] (rpc "ping" (object []))) app
      simpleStatus res `shouldBe` status403

  describe "other methods on /mcp" $ do
    it "rejects GET with 405" $ \app -> do
      res <- runSession (request (setPath defaultRequest { requestMethod = methodGet } "/mcp")) app
      simpleStatus res `shouldBe` status405
    it "rejects DELETE with 405" $ \app -> do
      res <- runSession (request (setPath defaultRequest { requestMethod = methodDelete } "/mcp")) app
      simpleStatus res `shouldBe` status405
    it "serves an info line on /" $ \app -> do
      res <- runSession (request (setPath defaultRequest "/")) app
      simpleStatus res `shouldBe` status200
      simpleBody res `shouldSatisfy` BL.isPrefixOf "smcdel-mcp"

  describe "originAllowed" $ do
    it "accepts localhost variants" $ \_ -> do
      originAllowed "http://localhost" `shouldBe` True
      originAllowed "http://127.0.0.1:8080" `shouldBe` True
      originAllowed "https://[::1]:3000/" `shouldBe` True
    it "rejects lookalike hosts" $ \_ -> do
      originAllowed "http://localhost.evil.com" `shouldBe` False
      originAllowed "http://127.0.0.1.evil.com" `shouldBe` False

postJson :: ToJSON a => [(HeaderName, BS.ByteString)] -> a -> Session SResponse
postJson extra payload = postRaw extra (encode payload)

postRaw :: [(HeaderName, BS.ByteString)] -> BL.ByteString -> Session SResponse
postRaw extra payload = srequest SRequest
  { simpleRequest = setPath defaultRequest
      { requestMethod = methodPost
      , requestHeaders = (hContentType, "application/json") : (hAccept, "application/json, text/event-stream") : extra
      } "/mcp"
  , simpleRequestBody = payload
  }
