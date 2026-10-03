{-# LANGUAGE OverloadedStrings #-}
module ProtocolSpec (spec) where

import Data.Aeson
import qualified Data.Text as T
import Test.Hspec

import Helpers
import SMCDEL.MCP.Protocol

spec :: Spec
spec = beforeAll (newServerConfig 30) $ do
  describe "JSON-RPC handling" $ do
    it "answers invalid JSON with a parse error" $ \cfg -> do
      reply <- handleBytes cfg "this is not json"
      (asInt . path ["error","code"] =<< reply) `shouldBe` Just (-32700)
      (path ["id"] =<< reply) `shouldBe` Just Null
    it "rejects messages without jsonrpc 2.0" $ \cfg -> do
      reply <- handleMessage cfg (object [ "id" .= (1 :: Int), "method" .= ("ping" :: T.Text) ])
      (asInt . path ["error","code"] =<< reply) `shouldBe` Just (-32600)
    it "does not reply to notifications" $ \cfg -> do
      reply <- handleMessage cfg (object [ "jsonrpc" .= ("2.0" :: T.Text), "method" .= ("notifications/initialized" :: T.Text) ])
      reply `shouldBe` Nothing
    it "does not reply to client responses" $ \cfg -> do
      reply <- handleMessage cfg (object [ "jsonrpc" .= ("2.0" :: T.Text), "id" .= (7 :: Int), "result" .= object [] ])
      reply `shouldBe` Nothing
    it "answers unknown methods with method-not-found" $ \cfg -> do
      reply <- call cfg "resources/list" (object [])
      asInt (path ["error","code"] reply) `shouldBe` Just (-32601)
      path ["id"] reply `shouldBe` Just (Number 1)

  describe "initialize" $ do
    it "echoes a supported protocol version and announces tools" $ \cfg -> do
      reply <- call cfg "initialize" (object
        [ "protocolVersion" .= ("2025-03-26" :: T.Text)
        , "capabilities" .= object []
        , "clientInfo" .= object [ "name" .= ("test" :: T.Text), "version" .= ("0" :: T.Text) ] ])
      asText (path ["result","protocolVersion"] reply) `shouldBe` Just "2025-03-26"
      path ["result","capabilities","tools"] reply `shouldSatisfy` (/= Nothing)
      asText (path ["result","serverInfo","name"] reply) `shouldBe` Just "smcdel-mcp"
      fmap (T.isInfixOf "VARS") (asText (path ["result","instructions"] reply)) `shouldBe` Just True
    it "falls back to the default version for unknown versions" $ \cfg -> do
      reply <- call cfg "initialize" (object [ "protocolVersion" .= ("1999-01-01" :: T.Text) ])
      asText (path ["result","protocolVersion"] reply) `shouldBe` Just defaultProtocolVersion
    it "answers ping with an empty object" $ \cfg -> do
      reply <- call cfg "ping" (object [])
      path ["result"] reply `shouldBe` Just (object [])

  describe "tools/list" $
    it "lists the three query tools with schemas" $ \cfg -> do
      reply <- call cfg "tools/list" (object [])
      let ts = maybe [] id (asList (path ["result","tools"] reply))
      map (asText . (.? "name")) ts `shouldBe` map Just [ "smcdel_true", "smcdel_valid", "smcdel_where" ]
      mapM_ (\t -> t .? "inputSchema" `shouldSatisfy` (/= Nothing)) ts
      mapM_ (\t -> t .? "outputSchema" `shouldSatisfy` (/= Nothing)) ts

  describe "tools/call errors" $ do
    it "rejects unknown tools with invalid params" $ \cfg -> do
      reply <- call cfg "tools/call" (object [ "name" .= ("smcdel_update" :: T.Text), "arguments" .= object [] ])
      asInt (path ["error","code"] reply) `shouldBe` Just (-32602)
    it "rejects missing arguments with invalid params" $ \cfg -> do
      reply <- call cfg "tools/call" (object [ "name" .= ("smcdel_valid" :: T.Text), "arguments" .= object [ "structure" .= muddyChildren ] ])
      asInt (path ["error","code"] reply) `shouldBe` Just (-32602)
    it "rejects a missing state for smcdel_true" $ \cfg -> do
      reply <- call cfg "tools/call" (object [ "name" .= ("smcdel_true" :: T.Text), "arguments" .= object [ "structure" .= muddyChildren, "formula" .= ("Top" :: T.Text) ] ])
      asInt (path ["error","code"] reply) `shouldBe` Just (-32602)

  describe "smcdel_true" $ do
    it "evaluates the muddy children example to true" $ \cfg -> do
      r <- callTool cfg "smcdel_true"
        [ "structure" .= muddyChildren, "state" .= [1,2,3 :: Int]
        , "formula" .= ("alice knows that (2 & 3) & ~ alice knows whether 1" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just False
      asBool (path ["structuredContent","holds"] r) `shouldBe` Just True
      fmap (T.isInfixOf "True") (firstText r) `shouldBe` Just True
    it "evaluates a false formula to false" $ \cfg -> do
      r <- callTool cfg "smcdel_true"
        [ "structure" .= muddyChildren, "state" .= [1,2,3 :: Int], "formula" .= ("alice knows whether 1" :: T.Text) ]
      asBool (path ["structuredContent","holds"] r) `shouldBe` Just False
    it "normalises the state in the output" $ \cfg -> do
      r <- callTool cfg "smcdel_true"
        [ "structure" .= muddyChildren, "state" .= [3,1,3 :: Int], "formula" .= ("Top" :: T.Text) ]
      path ["structuredContent","state"] r `shouldBe` Just (toJSON [1,3 :: Int])
    it "refuses states that violate the law" $ \cfg -> do
      r <- callTool cfg "smcdel_true" [ "structure" .= minimal, "state" .= ([] :: [Int]), "formula" .= ("Top" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "does not satisfy the LAW") (firstText r) `shouldBe` Just True
    it "reports states with atoms outside VARS as sanity errors" $ \cfg -> do
      r <- callTool cfg "smcdel_true" [ "structure" .= minimal, "state" .= [5 :: Int], "formula" .= ("Top" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "Sanity check failed") (firstText r) `shouldBe` Just True

  describe "smcdel_valid" $ do
    it "confirms the Minimal example" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= minimal, "formula" .= ("[! 1] alice knows that (bob knows that 1)" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just False
      asBool (path ["structuredContent","holds"] r) `shouldBe` Just True
    it "confirms the muddy children ignorance formula" $ \cfg -> do
      r <- callTool cfg "smcdel_valid"
        [ "structure" .= muddyChildren
        , "formula" .= ("( ~ (alice knows whether 1) & ~ (bob knows whether 2) & ~ (carol knows whether 3) )" :: T.Text) ]
      asBool (path ["structuredContent","holds"] r) `shouldBe` Just True
    it "refutes an invalid formula" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= muddyChildren, "formula" .= ("1" :: T.Text) ]
      asBool (path ["structuredContent","holds"] r) `shouldBe` Just False
    it "reports parse errors in the formula with a position" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= muddyChildren, "formula" .= ("alice knows that (" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "Parse error in the 'formula' argument") (firstText r) `shouldBe` Just True
    it "reports lex errors in the structure with a position" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= ("VARS 1,2\nLAW Top\nOBS alice: 1 $" :: T.Text), "formula" .= ("Top" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "Lex error in the 'structure' argument at line 3") (firstText r) `shouldBe` Just True
    it "reports unknown agents as sanity errors" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= muddyChildren, "formula" .= ("dave knows that 1" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "agents not in OBS") (firstText r) `shouldBe` Just True
    it "rejects structures that contain queries" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= (muddyChildren <> "VALID? Top"), "formula" .= ("Top" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "must only contain VARS, LAW and OBS") (firstText r) `shouldBe` Just True
    it "reports a non-Boolean LAW as a runtime error instead of crashing" $ \cfg -> do
      r <- callTool cfg "smcdel_valid" [ "structure" .= ("VARS 1\nLAW alice knows that 1\nOBS alice: 1" :: T.Text), "formula" .= ("Top" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just True
      fmap (T.isInfixOf "SMCDEL error") (firstText r) `shouldBe` Just True

  describe "smcdel_where" $ do
    it "finds the states where exactly one child knows after the announcement" $ \cfg -> do
      r <- callTool cfg "smcdel_where"
        [ "structure" .= muddyChildren
        , "formula" .= ("< ! (1|2|3) > ( (alice knows whether 1) | (bob knows whether 2) | (carol knows whether 3) )" :: T.Text) ]
      asBool (path ["isError"] r) `shouldBe` Just False
      path ["structuredContent","states"] r `shouldBe` Just (toJSON [[1],[2],[3 :: Int]])
      asInt (path ["structuredContent","count"] r) `shouldBe` Just 3
      asBool (path ["structuredContent","truncated"] r) `shouldBe` Just False
    it "truncates the list but keeps the exact count" $ \cfg -> do
      r <- callTool cfg "smcdel_where" [ "structure" .= muddyChildren, "formula" .= ("Top" :: T.Text), "maxStates" .= (2 :: Int) ]
      asInt (path ["structuredContent","count"] r) `shouldBe` Just 8
      fmap length (asList (path ["structuredContent","states"] r)) `shouldBe` Just 2
      asBool (path ["structuredContent","truncated"] r) `shouldBe` Just True
    it "lists all states by default" $ \cfg -> do
      r <- callTool cfg "smcdel_where" [ "structure" .= muddyChildren, "formula" .= ("Top" :: T.Text) ]
      fmap length (asList (path ["structuredContent","states"] r)) `shouldBe` Just 8
      asBool (path ["structuredContent","truncated"] r) `shouldBe` Just False
    it "returns no states for an unsatisfiable formula" $ \cfg -> do
      r <- callTool cfg "smcdel_where" [ "structure" .= minimal, "formula" .= ("~1 & ~2" :: T.Text) ]
      path ["structuredContent","states"] r `shouldBe` Just (toJSON ([] :: [[Int]]))
      asInt (path ["structuredContent","count"] r) `shouldBe` Just 0

-- | The text of the first content block of a tool result.
firstText :: Value -> Maybe T.Text
firstText r = do
  blocks <- asList (path ["content"] r)
  case blocks of
    (b:_) -> asText (b .? "text")
    []    -> Nothing
